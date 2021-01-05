package de.cispa.se.tribble
package model

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.LiteralMerge

class LiteralMergeSpec extends TestSpecification {
  private val literalMerge = new LiteralMerge

  "The LiteralMerge" should "merge concatenations correctly" in {
    val grammars = Table(("input grammar", "merged grammar"),
      ("S", Map('S := "a" ~ "b")) -> ("S", Map('S := "ab")),
      ("S", Map('S := "a" ~ "b" ~ "c")) -> ("S", Map('S := "abc")),
      ("S", Map('S := "a" ~ ("b" ~ "c"))) -> ("S", Map('S := "abc")),
      ("S", Map('S := ("a" ~ ("b" ~ "c")))) -> ("S", Map('S := "abc")),
      ("S", Map('S := (("a" ~ "b") ~ "c"))) -> ("S", Map('S := "abc")),
      ("S", Map('S := (("a" ~ "b") ~ "c" ~ "d"))) -> ("S", Map('S := "abcd")),
      ("S", Map('S := (("a" ~ "b") ~ ("c" ~ "d")))) -> ("S", Map('S := "abcd")),
      ("S", Map('S := ("a" ~ ("b" ~ "c") ~ "d"))) -> ("S", Map('S := "abcd")),
    )

    forAll(grammars) { (prods, expected) =>
      literalMerge.process(GrammarRepr(prods._1, prods._2)) shouldEqual GrammarRepr(expected._1, expected._2)
    }
  }

  it should "merge references correctly" in {
    val grammars = Table(("input grammar", "merged grammar"),
      ("S", Map('S := "a" ~ 'A, 'A := "b")) -> ("S", Map('S := "ab")),
      ("S", Map('S := "a" ~ 'A, 'A := "b" ~ 'C, 'C := "c")) -> ("S", Map('S := "abc")),
      ("S", Map('S := "a" ~ 'A, 'A := "b" ~ 'C, 'C := "c" ~ 'D, 'D := "d")) -> ("S", Map('S := "abcd")),
      // unfortunately we are not smart enough to handle the following case
      // besides, the two "b"s will have different ids anyway
      ("S", Map('S := "a" ~ ('A | 'B), 'A := "b", 'B := "b")) -> ("S", Map('S := "a" ~ ('A | 'B), 'A := "b", 'B := "b")),
    )

    forAll(grammars) { (prods, expected) =>
      literalMerge.process(GrammarRepr(prods._1, prods._2)) shouldEqual GrammarRepr(expected._1, expected._2)
    }
  }

  it should "merge combinations of literals and references correctly" in {
    val grammars = Table(("input grammar", "merged grammar"),
      ("S", Map('S := "a" ~ 'A, 'A := "b")) -> ("S", Map('S := "ab")),
      ("S", Map('S := "a" ~ "b" ~ "c" ~ 'A, 'A := "d")) -> ("S", Map('S := "abcd")),
      ("S", Map('S := "a" ~ 'A, 'A := "b" ~ "c")) -> ("S", Map('S := "abc")),
      ("S", Map('S := "a" ~ "b" ~ 'A, 'A := "c")) -> ("S", Map('S := "abc")),
      ("S", Map('S := "a" ~ 'A, 'A := "b" ~ 'C, 'C := "c" ~ "d")) -> ("S", Map('S := "abcd")),
      // todo this is not inlined all the way
      // ("A", Map('A := "a" ~ 'B | "x", 'B := "b" ~ "c" ~ 'D, 'D := "d" ~ 'A)) -> ("S", Map('A := "abcd" ~ 'A | "x")),
    )

    forAll(grammars) { (prods, expected) =>
      literalMerge.process(GrammarRepr(prods._1, prods._2)) shouldEqual GrammarRepr(expected._1, expected._2)
    }
  }

  it should "merge literals correctly in the context of other DerivationRules" in {
    val grammars = Table(("input grammar", "merged grammar"),
      ("S", Map('S := ("a" ~ 'A).?, 'A := "b")) -> ("S", Map('S := "ab".?)),
      ("S", Map('S := ("a" ~ 'A).rep, 'A := "b")) -> ("S", Map('S := "ab".rep)),
      ("S", Map('S := ("a" ~ 'A).rep(1), 'A := "b")) -> ("S", Map('S := "ab".rep(1))),
      ("S", Map('S := ("a" ~ 'A).rep(2,5), 'A := "b")) -> ("S", Map('S := "ab".rep(2,5))),
      ("S", Map('S := "a" ~ 'A | "z", 'A := "b")) -> ("S", Map('S := "ab" | "z")),
      ("S", Map('S := "a" ~ 'A | "z" ~ 'A, 'A := "b")) -> ("S", Map('S := "ab" | "zb")),
      ("A", Map('A := "a" ~ 'B | "x", 'B := "b" ~ "c" ~ 'D, 'D := "d")) -> ("A", Map('A := "abcd" | "x")),
      ("A", Map('D := "d", 'B := "b" ~ "c" ~ 'D, 'A := "a" ~ 'B | "x")) -> ("A", Map('A := "abcd" | "x")),
      ("A", Map('D := "d", 'A := "a" ~ 'B | "x", 'B := "b" ~ "c" ~ 'D)) -> ("A", Map('A := "abcd" | "x")),
    )

    forAll(grammars) { (prods, expected) =>
      literalMerge.process(GrammarRepr(prods._1, prods._2)) shouldEqual GrammarRepr(expected._1, expected._2)
    }
  }
}
