package de.cispa.se.tribble
package input

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.TestDSL._

class ModelAssemblerSpec extends TestSpecification with SharedModelAssembler with SharedAutomatonCache {

  "The ModelAssembler" should "assemble simple grammars correctly" in {
    val grammars = Table(("productions", "grammar"),
      Seq('S := "a") -> ("S", Map('S := "a"/0)),
      Seq('A := 'B, 'B := "b") -> ("A", Map('A := 'B/0, 'B := "b"/1)),
      Seq('A := "a" ~ 'B, 'B := "b") -> ("A", Map('A := ("a"/ 1 -- 'B/2)/0, 'B := "b"/3))
    )

    forAll(grammars) { (prods, expected) =>
      modelAssembler.assemble(prods) shouldEqual GrammarRepr(expected._1, expected._2)
    }
  }

  it should "detect invalid grammars" in {
    val grammars = Table(("grammar", "message"),
      Seq('S := 'S) -> "Grammar contains no root symbol",
      Seq('A := 'B, 'B := 'A) -> "Grammar contains no root symbol",
      Seq('A := 'B, 'B := 'C, 'C := 'D, 'D := 'A) -> "Grammar contains no root symbol",
      Seq('A := "a", 'B := "b") -> "Grammar contains multiple root symbols",
      Seq('A := "a" ~ 'C) -> "Grammar contains undefined symbols",
      Seq('A := "a" ~ 'C.?) -> "Grammar contains undefined symbols",
      Seq('A := "a" ~ 'C.rep(1)) -> "Grammar contains undefined symbols",
      Seq('A := "a" ~ 'C.rep) -> "Grammar contains undefined symbols",
      Seq('A := "a" ~ 'C.rep(2, 8)) -> "Grammar contains undefined symbols",
      Seq('A := "a" ~ 'C.rep(0, 2)) -> "Grammar contains undefined symbols",
      Seq('A := "a" ~ 'C.rep(0, Int.MaxValue)) -> "Grammar contains undefined symbols",
      Seq('A := "a", 'A := "b") -> "Cannot have multiple declarations for A",
      Seq('A := "a", 'A := "a") -> "Cannot have multiple declarations for A"
    )

    forAll(grammars) { (grammar, errorMessage) =>
      val iae = the[IllegalArgumentException] thrownBy modelAssembler.assemble(grammar)
      iae.getMessage should startWith(errorMessage)
    }
  }

  it should "properly assign (and scale up) probabilities" in {
    val grammars = Table("grammar",
      Seq('A := "a" @@ 0.5 | "b"),
      Seq('A := "a" @@ 0.5 | "b" @@ -0.0d),
      Seq('A := "a" @@ 0.5 | "b" @@ -0.0f),
      Seq('A := "a" @@ 0.5 | "b" @@ -0),
      Seq('A := "a" @@ 0.5 | "b" @@ .99 ~ "c"),
      Seq('A := "a" @@ 0.1 | "b" @@ (Math.PI / 4)),
      Seq('A := "a" | "b"),
      Seq('A := "a" | "b" @@ 0.5),
      Seq('A := "a" | "b" @@ 0.3),
      Seq('A := "a" ~ "c" | "b" @@ 0.3),
      Seq('A := "a" @@ .9 ~ "c" | "b" @@ 0.3),
      Seq('A := "a" @@ .9 ~ "c" @@ .5 | "b" @@ 0.5),
      Seq('A := "a" ~ "c" @@ .5 | "b" @@ 0.5d),
      Seq('A := "a" | "b" | "c"),
      Seq('A := "a" @@ .1 | "b" | "c"),
      Seq('A := "a" | "b" @@ .1 | "c"),
      Seq('A := "a" | "b" | "c" @@ .1),
      Seq('A := "a" | "b" @@ .5 | "c" @@ .5),
      Seq('A := "a" @@ .0 | "b" @@ 0 | "c" @@ .5),
      Seq('A := "a" @@ 000.25 | "b" @@ 0 | "c" @@ .25)
    )

    forAll (grammars) {grammar =>
      inside (modelAssembler.assemble(grammar)("A")) { case Alternation(alts, _) =>
        alts.map(_.probability).sum === 1.0 +- 1E-9
      }
    }
  }

  it should "detect invalid probabilities" in {
    val grammars = Table("grammar",
      Seq('A := "a" @@ 0.8 | "b" @@ 0.9),
      Seq('A := "a" @@ 0.33 | "b" @@ 0.33 | "c" ~ "d" @@ 0.5),
      Seq('A := "a" @@ 0.667 | "b" @@ .33 ~ "c" @@ .3333)
    )

    val assembler = new ModelAssembler(automatonCache)
    forAll(grammars) { grammar =>
      an [IllegalArgumentException] should be thrownBy assembler.assemble(grammar)
    }

  }

  it should "be able to eliminate all instances of Int.MaxValue from shortest derivations" in {
    val grammars = Table("grammar",
      Seq('S := 'A,
        'A := "s" | "s" ~ 'A),
      Seq('S := 'A.rep(1),
        'A := "a"),
      Seq('S := 'A,
        'A := 'B,
        'B := "b"),
      Seq('S := 'A ~ 'B,
        'A := ("a" ~ 'A) | "a",
        'B := 'A | ('A ~ 'B))
    )

    forAll(grammars) { g =>
      val grammar = modelAssembler.assemble(g)
      grammar.rules.values.flatMap(_.toStream).map(_.shortestDerivation) should not contain Int.MaxValue
    }
  }

  it should "handle id 0 manually set" in {
    val g = Grammar(
      'S := 'A -- 'B -- 'C,
      'A := "a",
      'B := "b"/0,
      'C := "c"
    )

    val grammar = modelAssembler.assemble(g.productions)
    val assignedIds = grammar.rules.values.flatMap(_.toStream).map(_.id).toSet
    assignedIds should contain only(0, 1, 2, 3, 4, 5, 6)
    grammar("B").id shouldEqual 0
  }

  it should "handle arbitrarily set ids" in {
    val g = Grammar(
      'S := 'A/5 -- 'B/2 -- 'C/7,
      'A := "a",
      'B := "b",
      'C := "c"
    )

    val grammar = modelAssembler.assemble(g.productions)
    val assignedIds = grammar.rules.values.flatMap(_.toStream).map(_.id).toSet
    assignedIds should have size 7
    assignedIds should contain only(0, 1, 2, 3, 4, 5, 7)
    grammar.root.children.map(_.id).toList should contain inOrderOnly(5, 2, 7)
  }

  it should "detect duplicate manually set ids" in {
    val g = Grammar(
      'S := 'A/42 -- 'B/42,
      'A := "a",
      'B := "b"
    )

    an[IllegalArgumentException] should be thrownBy modelAssembler.assemble(g.productions)
  }

  it should "detect a terminal root symbol" in {
    val g = Grammar(
      'S := "s",

      'A := 'B.rep,
      'B := "b" ~ 'C,
      'C := 'A | "c"
    )

    val iae = the[IllegalArgumentException] thrownBy modelAssembler.assemble(g.productions)
    iae.getMessage shouldEqual "When the root of the grammar is a terminal symbol, it is not allowed to have other productions!"
  }

  it should "detect unconnected cross-referencing self loops" in {
    val g = Grammar(
      'S := 'X | 'Y,
      'X := "x",
      'Y := "y",

      'A := "a" | 'B,
      'B := "b" | 'A
    )

    val iae = the[IllegalArgumentException] thrownBy modelAssembler.assemble(g.productions)
    iae.getMessage should startWith("Grammar contains symbols unreachable from the root")
  }

  // TODO do something about the fact that the following grammar hangs the process indefinitely
  // Seq('S := 'A, 'A := 'B, 'B := 'A)

}
