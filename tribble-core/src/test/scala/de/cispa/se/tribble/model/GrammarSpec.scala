package de.cispa.se.tribble
package model

import de.cispa.se.tribble.dsl._

class GrammarSpec extends TestSpecification {

  private val grammar: GrammarRepr = GrammarRepr("A", Map('A := 'B, 'B := "b"))

  "The Grammar" should "be usable like a map" in {
    val b: Reference = 'B
    grammar.get("A") shouldEqual b
    grammar.get(b) shouldEqual Literal("b")
  }


  it should "disallow constructing invalid probabilities" in {
    val probabilities = Table("probability", -0.5, Math.PI, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)

    forAll (probabilities) { p =>
      an [IllegalArgumentException] should be thrownBy Grammar('A := "a" @@ p)
    }
  }
}
