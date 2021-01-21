package de.cispa.se.tribble
package model

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.TestDSL._

class GrammarSpec extends TestSpecification {

  private val grammar: GrammarRepr = GrammarRepr("A", Map('A := 'B, 'B := "b"))

  "The Grammar" should "be usable like a map" in {
    val b: Reference = 'B
    grammar("A") shouldEqual b
    grammar(b) shouldEqual Literal("b")
  }


  it should "disallow constructing invalid probabilities" in {
    val probabilities = Table("probability", -0.5, Math.PI, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)

    forAll (probabilities) { p =>
      an [IllegalArgumentException] should be thrownBy Grammar('A := "a" @@ p)
    }
  }

  "Derivation rules" should "be compared by properly considering the id" in {
    shouldEqual('a, 'a)
    shouldEqual('a/1, 'a/1)
    shouldNotEqual('a/1, 'a/2)
    shouldNotEqual('a/1, 'b/1)

    shouldEqual("a"/1, "a"/1)
    shouldNotEqual("a"/1, "b"/2)
    shouldNotEqual("a"/1, "b"/1)

    shouldEqual('a/1 -- 'b/2, 'a/1 -- 'b/2)
    shouldNotEqual('a/1 -- 'b/2, 'a/1 -- "b"/2)

    shouldEqual('a/1 | 'b/2, 'a/1 | 'b/2)
    shouldNotEqual('a/1 | 'b/2, 'a/1 | "b"/2)
    shouldNotEqual('a/1 -- 'b/2, 'a/1 | 'b/2)
  }

  // these helper methods exist so that the equals methods are resolved to the DerivationRule trait
  private def shouldEqual(a: DerivationRule, b: DerivationRule) = a shouldEqual b

  private def shouldNotEqual(a: DerivationRule, b: DerivationRule) = a shouldNot equal(b)
}
