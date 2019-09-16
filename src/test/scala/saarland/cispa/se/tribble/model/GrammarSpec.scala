package saarland.cispa.se.tribble
package model

import saarland.cispa.se.tribble.dsl._

class GrammarSpec extends TestSpecification {

  private val grammar: GrammarRepr = ("A", Map('A := 'B, 'B := "b"))

  "The Grammar" should "be usable like a map" in {
    val b: Reference = 'B
    grammar("A") shouldEqual b
    grammar(b) shouldEqual Literal("b")
  }

  it should "have an appropriate string representation" in {
    grammar.pprint shouldEqual
      """--> A
        |A ::= 'B0;
        |B ::= "b"0;""".stripMargin
  }
}
