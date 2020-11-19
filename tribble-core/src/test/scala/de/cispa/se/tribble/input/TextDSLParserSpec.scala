package de.cispa.se.tribble
package input

import de.cispa.se.tribble.dsl._
import fastparse._

class TextDSLParserSpec extends TestSpecification {

  "The text grammar parser" should "parse simple rules" in {
    val rules = Table(
      "grammar" -> "structure",
      // a single simple rule
      """S = "hello";""" -> Seq('S := "hello"),
      """S = r"hello";""" -> Seq('S := 'r ~ "hello"),
      """S = r "hello";""" -> Seq('S := 'r ~ "hello"),
      // longer non-terminal
      """Hello = "world"; """ -> Seq('Hello := "world"),
      // two rules in one line
      """A = "a"; B = "b";""" -> Seq('A := "a", 'B := "b"),
      // epsilon
      """E = ""; """ -> Seq('E := ""),
      // self loop
      """S = S; """ -> Seq('S := 'S),
      // extraneous whitespace
      """
        |A  =     "a"    ;
        |
      """.stripMargin -> Seq('A := "a"),
      // two simple rules with a line break
      """
        |A = "a";
        |B = "b";
      """.stripMargin -> Seq('A := "a", 'B := "b"),
      // cross recursion
      """A=B;B=A;""" -> Seq('A := 'B, 'B := 'A)

    )

    forAll(rules) { (grammar, structure) =>
      parse(grammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(`structure`, _) => }
    }

  }

  it should "parse string escapes correctly" in {
    val escapes = Table(
      "input" -> "expected",
      """S = "a";""" -> "a",
      """S = "\"";""" -> "\"",
      """S = "\t";""" -> "\t",
      """S = "\tb";""" -> "\tb",
      """S = "a\tb";""" -> "a\tb",
      """S = "a\"b";""" -> "a\"b",
      """S = "a\\b";""" -> "a\\b",
      """S = "\"\\";""" -> "\"\\",
      """S = "a\nb";""" -> "a\nb",
      """S = "a\rb";""" -> "a\rb",
      """S = "a\r\nb";""" -> "a\r\nb",
      """S = "a\n\rb";""" -> "a\n\rb",
      """S = "a\"\\\t\r\nb";""" -> "a\"\\\t\r\nb"
    )

    forAll(escapes) { (grammar, expected) =>
      inside(parse(grammar, TextDSLParser.grammar(_))) { case Parsed.Success(Seq(("S", Literal(actual,_))), _) =>
        actual shouldEqual expected
      }
    }
  }

  it should "parse unescaped string literals correctly" in {
    val literals = Table(
      "input" -> "expected",
      "S = \" \";" -> " ",
      "S = \"\t\";" -> "\t",
      "S = \"\";" -> "",
      """S = "a
        |b";""".stripMargin -> "a\nb",
      """S = "a
        |
        |
        |b";""".stripMargin -> "a\n\n\nb"
    )

    forAll(literals) { (grammar, expected) =>
      inside(parse(grammar, TextDSLParser.grammar(_))) { case Parsed.Success(Seq(("S", Literal(actual,_))), _) =>
        actual shouldEqual expected
      }
    }
  }

  it should "parse quantifications correctly" in {
    val quants = Table(
      "grammar" -> "structure",
      """S = "a"?; """ -> Seq('S := "a".?),
      """S = "a"
        |?; """.stripMargin -> Seq('S := "a".?),
      """S = "a"+; """ -> Seq('S := "a".rep(1)),
      """S = "a"   *; """ -> Seq('S := "a".rep),
      """S = "a"{,1}; """ -> Seq('S := "a".?),
      """S = "a"      {,1}; """ -> Seq('S := "a".?),
      """S = "a"{0,1}; """ -> Seq('S := "a".?),
      """S = "a"{0  ,
        |   1}; """.stripMargin -> Seq('S := "a".?),
      """S = "a"{0,}; """ -> Seq('S := "a".rep),
      """S = "a"
        |
        |{0,}; """.stripMargin -> Seq('S := "a".rep),
      """S = "a"{1,}; """ -> Seq('S := "a".rep(1)),
      """S = "a" {  2 }; """ -> Seq('S := "a".rep(2, 2)),
      """S = "a" { 2,2 }; """ -> Seq('S := "a".rep(2, 2)),
      """S = "a"{2,5}; """ -> Seq('S := "a".rep(2, 5)),
      """S = "a"{2,5
        |}; """.stripMargin -> Seq('S := "a".rep(2, 5))
    )

    forAll(quants) { (grammar, structure) =>
      parse(grammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(`structure`, _) => }
    }
  }

  it should "parse concatenations correctly" in {
    val concats = Table(
      "grammar" -> "structure",
      """S = "a" "b"; """ -> Seq('S := "a" ~ "b"),
      """S = "a" "b" "c"
        | "d" "e"
        | "f"; """.stripMargin -> Seq('S := "a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f"),
      """S = ("a" "b"); """ -> Seq('S := "a" ~ "b"),
      """S = ("a" "b"){2,5}; """ -> Seq('S := ("a" ~ "b").rep(2, 5)),
      """S = "a" ("b"); """ -> Seq('S := "a" ~ "b"),
      """S = ("a") "b"; """ -> Seq('S := "a" ~ "b"),
      """S = ("a") (A); """ -> Seq('S := "a" ~ 'A),
      """S = (("a") ("b")); """ -> Seq('S := "a" ~ "b"),
      """S = "" ("b" "c"); """ -> Seq('S := Concatenation(Seq("", "b" ~ "c"))),
      """S = ("a" "b") "c"; """ -> Seq('S := Concatenation(Seq("a" ~ "b", "c"))),
      """S = "a" ("b" "c") "d"; """ -> Seq('S := Concatenation(Seq("a", "b" ~ "c", "d")))
    )

    forAll(concats) { (grammar, structure) =>
      parse(grammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(`structure`, _) => }
    }
  }

  it should "parse alternations correctly" in {
    val alts = Table(
      "grammar" -> "structure",
      """S = "a"| "b"; """ -> Seq('S := "a" | "b"),
      """S = "a" |"b"?; """ -> Seq('S := "a" | "b".?),
      """S = ("a"|"b")+; """ -> Seq('S := ("a" | "b").rep(1)),
      """S = "$"|"€"|"%"; """ -> Seq('S := "$" | "€" | "%"),
      """S = "$" "€"|"%"; """ -> Seq('S := "$" ~ "€" | "%"),
      """S = "a"
        |"b"|C; """.stripMargin -> Seq('S := "a" ~ "b" | 'C),
      """S = "a" ("b"?|"c")
        |+; """.stripMargin -> Seq('S := "a" ~ ("b".? | "c").rep(1)),
      """S = "a" "b" |
        |"c"; """.stripMargin -> Seq('S := "a" ~ "b" | "c"),
      """S
        |= "a"*   "b"
        |   |"c"; """.stripMargin -> Seq('S := "a".rep ~ "b" | "c")
    )

    forAll(alts) { (grammar, structure) =>
      parse(grammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(`structure`, _) => }
    }
  }

  it should "parse derivation rules annotated with probabilities" in {
    val id = DerivationRule.DEFAULT_ID
    val grammars = Table("grammar" -> "expected",
      """S = "a" @@ 0.5 | "b" @@ 0.5;""" -> s"""("a"$id@@0.5 | "b"$id@@0.5)a$id""",
      """S = "a" @@ 0.04 | "b";""" -> s"""("a"$id@@0.04 | "b"$id)a$id""",
      """S = "a" @@ 0.6 | "b" "c" @@ 0.4;""" -> s"""("a"$id@@0.6 | ("b"$id ~ "c"$id)c$id@@0.4)a$id""",
      """S = "a" @@ 0x1.0p0 | "b";""" -> s"""("a"$id@@1.0 | "b"$id)a$id"""
    )

    forAll(grammars) { (grammar, exp) =>
      val value = parse(grammar, TextDSLParser.grammar(_))
      val Parsed.Success(g, _) = value
      g.head._2.toString shouldBe exp
    }
  }

  it should "also merge equal alternations" in {
    val alts = Table(
      "grammar" -> "structure",
      """S = "a"| "a"; """ -> Seq('S := "a"),
      """S = "a" |"a"?; """ -> Seq('S := "a" | "a".?),
      """S = "a"?|"a"?; """ -> Seq('S := "a".?),
      """S = ("a"|"a")+; """ -> Seq('S := "a".rep(1)),
      """S = ("a" | "a" | "a" | "a" | "a")+; """ -> Seq('S := "a".rep(1)),
      """S = "$" "€"| ("$"  ("€")); """ -> Seq('S := "$" ~ "€")
    )

    forAll(alts) { (grammar, structure) =>
      parse(grammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(`structure`, _) => }
    }
  }

  it should "ignore comments" in {
    val comments = Table("grammar",
      "S = a; /* hello */",
      "S/**/ = a;",
      "S/***/ = a;",
      "S= /*hi*/a;",
      "S= a;/*hu*/",
      "S= /*hi*/a;/*hu*/",
      "S= /*hi*//**/a;",
      "S= a/*hi**/;",
      "S= a/*hi****/;",
      "S= a/*hi* * * */;",
      """S= a/*
        |hi*
        |* * */;""".stripMargin,
      "S = a; // hello",
      "S = a; // hello    ",
      """S = a; // hello
        |
      """.stripMargin,
      """S =     // hello
        |a /*-.-*/; //
      """.stripMargin,
      """S = a; // /*
        |
      """.stripMargin,
      """S = a; //*
        |
      """.stripMargin

    )

    val nt = 'S := 'a
    forAll(comments) {
      parse(_, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(Seq(`nt`), _) => }
    }
  }

  it should "ignore comments in literals and quants" in {
    val moreComments = Table("grammar" -> "expected",
      """S="a/*hi*/b";""" -> Seq('S := "a/*hi*/b"),
      "S=a/*0*/?;" -> Seq('S := 'a.?),
      "S=a/*0*/+;" -> Seq('S := 'a.rep(1)),
      "S=a/*0*/*;" -> Seq('S := 'a.rep),
      "S=a/*0*//**/*;" -> Seq('S := 'a.rep),
      "S=a{/*0*/2};" -> Seq('S := 'a.rep(2, 2)),
      "S=a{/*0*/2/**/};" -> Seq('S := 'a.rep(2, 2)),
      "S=a{2/*3*/};" -> Seq('S := 'a.rep(2, 2)),
      "S=a{2/*3*/,};" -> Seq('S := 'a.rep(2, Int.MaxValue)),
      "S=a{/*4*/5,};" -> Seq('S := 'a.rep(5, Int.MaxValue)),
      "S=a{,/*4*/5};" -> Seq('S := 'a.rep(0, 5)),
      "S=a{/**/,/*,,*/5};" -> Seq('S := 'a.rep(0, 5)),
      "S=a{2/**/,/*,,*/5};" -> Seq('S := 'a.rep(2, 5)),
      "S=a{/*hi*/2/**/,/*,,*/5};" -> Seq('S := 'a.rep(2, 5)),
      """S=a{/*h
        |i*/2
        |/*
        |*/,/*,,*/
        |5}
        |;""".stripMargin -> Seq('S := 'a.rep(2, 5)),
      """S=a{/*h
        |i*/2 // :)
        |/*
        |*/,/*,,*/
        |5}
        |;//""".stripMargin -> Seq('S := 'a.rep(2, 5))
    )

    forAll(moreComments) { (grammar, structure) =>
      parse(grammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Success(`structure`, _) => }
    }
  }

  it should "parse regular expressions correctly" in {
    val regexes = Table(("grammar", "expected"),
      "S = /hello/;" -> "hello",
      "S = /a /;" -> "a ",
      "S = / a/;" -> " a",
      "S = /a+/;" -> "a+",
      "S = /a*/;" -> "a*",
      "S = /a?/;" -> "a?",
      "S = /a|b/;" -> "a|b",
      "S = /[a-z]/;" -> "[a-z]",
      "S = /[^a-z]/;" -> "[^a-z]",
      "S = /\\/h/;" -> "/h",
      "S = /\\\\\\/h/;" -> "\\/h",
      "S = /\\//;" -> "/",
      "S = /\\\\h/;" -> "\\h",
      "S = /\\\\\\\"/;" -> "\\\"",
      "S = /hello///comment\n;" -> "hello",
      "S = /he\\/*ll*\\/o/;" -> "he/*ll*/o",
      "S = /hel\nlo/\n;" -> "hel\nlo"
      // todo add tests with & ~ \+ \*
    )

    forAll(regexes) { (grammar, expected) =>
      inside(parse(grammar, TextDSLParser.grammar(_))) {
        case Parsed.Success(Seq(("S", Regex(actual, _))), _) => actual shouldEqual expected
        case Parsed.Failure(lastParser, index, extra) => fail(s"Parser expected $lastParser at index $index!\n${extra.trace().aggregateMsg}")
      }
    }
  }

  it should "fail on incorrect inputs" in {
    val fails = Table("non-grammar",
      "",
      "=",
      "AAAAA",
      "A=",
      "A==b;",
      "A=b",
      "H elp=b;",
      "A=(b;",
      "A=(b(;",
      "A=b);",
      "A=(b));",
      "A=a(|b);",
      "A=a(|)b;",
      "A=(a|)b;",
      "A=|A;",
      "A=A|;",
      "A=A A|;",
      "A=A | A|;",
      "A=A || A;",
      "A=A;;",
      "A=;",
      "A;A=A;",
      ";A=A;",
      """A="hello; """,
      """A="hello;" """,
      """A="\a"; """,
      """A="\\\b"; """,
      """A="\f"; """,
      """A="\"; """,
      """A=\"; """,
      "A=?;",
      "A=??;",
      "A=*?;",
      "A=**;",
      "A=+*;",
      "A=++;",
      "A=+?;",
      "A=a??;",
      "A=a+*;",
      "A=a*?;",
      "A=a{};",
      "A=a{5 5};",
      "A=a{0};", // the parser prevents pointless zero-repetitions
      "A=a{0000};",
      "A=a{1};", // and pointless 1-repetitions
      "A=a{1,1};",
      "A=a{001};",
      "A=a{2,1};", // the parser also prevents min > max
      "A=a{2,01};",
      "A=a{002,01};",
      "A=a{002,1};",
      "A=a{,,};",
      "A=a{,,1};",
      "A=a{-2};",
      "A=a{+6};",
      "A=a{1,,};",
      "A=a{1 1,};",
      "S=a//;",
      "S=a/;",
      "//S=a;",
      "S=a;/*",
      "S=a{/**/};",
      "S=///;"
    )

    forAll(fails) { nonGrammar =>
      parse(nonGrammar, TextDSLParser.grammar(_)) should matchPattern { case Parsed.Failure(_, _, _) => }
    }
  }
}
