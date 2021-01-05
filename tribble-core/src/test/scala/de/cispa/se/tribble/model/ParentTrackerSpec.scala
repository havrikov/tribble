package de.cispa.se.tribble
package model

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.SharedModelAssembler

class ParentTrackerSpec extends TestSpecification with SharedModelAssembler {

  private val g = Grammar(
    'S := 'A | 'B | "c" ~ ('E | 'F ~ "foo".rep),
    'A := "a",
    'B := 'Q.? ~ 'R.rep(1, 5),
    'Q := "q",
    'R := "[a-z]".regex,
    'E := "e",
    'F := "f"
  )

  private val grammar: GrammarRepr = modelAssembler.assemble(g.productions)

  private val tracker = new ParentTracker(grammar)

  "The ParentTracker" should "correctly report None for top level declarations" in {
    for (elem <- g.productions.map(_._2)) {
      tracker.parent(elem) shouldBe None
    }
  }

  it should "throw on nonexistent rules" in {
    assertThrows[NoSuchElementException] {
      tracker.parent(Reference("X", 42))
    }
  }

  it should "have the correct number of parent entries" in {
    val numRules = g.productions.map(_._2).flatMap(_.toStream).size
    tracker.size shouldEqual numRules
  }

  it should "correctly report parents" in {
    val table = Table(("rule", "parent"),
      Literal("foo") -> "foo".rep,
      "foo".rep ->  'F ~ "foo".rep,
      'F ~ "foo".rep -> ('E | 'F ~ "foo".rep),
      ('E | 'F ~ "foo".rep) -> "c" ~ ('E | 'F ~ "foo".rep),
      "c" ~ ('E | 'F ~ "foo".rep) -> ('A | 'B | "c" ~ ('E | 'F ~ "foo".rep)),
      Reference("B") -> ('A | 'B | "c" ~ ('E | 'F ~ "foo".rep)),
      Reference("A") -> ('A | 'B | "c" ~ ('E | 'F ~ "foo".rep)),
      Reference("Q") -> 'Q.?,
      'Q.? -> 'Q.? ~ 'R.rep(1, 5),
    )

    forAll(table) { (rule, parent) =>
      tracker.parent(rule) shouldEqual Some(parent)
    }
  }
}
