package saarland.cispa.se.tribble
package input

import saarland.cispa.se.tribble.dsl._
import saarland.cispa.se.tribble.model.Alternation

class ModelAssemblerSpec extends TestSpecification with SharedModelAssembler {

  "The ModelAssembler" should "assemble simple grammars correctly" in {
    val grammars = Table(("productions", "grammar"),
      Seq('S := "a") -> ("S", Map('S := "a")),
      Seq('A := 'B, 'B := "b") -> ("A", Map('A := 'B, 'B := "b")),
      Seq('A := "a" ~ 'B, 'B := "b") -> ("A", Map('A := "a" ~ 'B, 'B := "b"))
    )

    forAll(grammars) { (prods, expected) =>
      modelAssembler.assemble(prods) shouldEqual expected
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
      grammar.map(_.shortestDerivation) should not contain Int.MaxValue
    }
  }

  // TODO do something about the fact that the following grammar hangs the process indefinitely
  // Seq('S := 'A, 'A := 'B, 'B := 'A)

}
