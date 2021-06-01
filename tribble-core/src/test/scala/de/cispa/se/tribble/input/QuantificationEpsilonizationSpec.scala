package de.cispa.se.tribble
package input

import de.cispa.se.tribble.TestDSL._
import de.cispa.se.tribble.dsl._

class QuantificationEpsilonizationSpec extends TestSpecification with SharedAutomatonCache {
  private val modelAssembler = new ModelAssembler(
    automatonCache,
    assignProbabilities = false,
    epsilonizeQuantifications = true
  )

  "The QuantificationEpsilonization" should "leave grammars without quantifications untouched" in {
    val g = Grammar(
      'A := "a" ~ 'B | 'C,
      'B := "b",
      'C := "c"
    )

    // The following should not throw an IllegalArgumentException
    modelAssembler.assemble(g.productions)
  }

  it should "transform quantifications" in {
    val rules = Table(("rule", "expected"),
      ("a"/0).? /42 ==> (""/2 | "a"/0)/1,
      ("b"/0).rep /42 ==> (""/2 | ("b"/0).rep(1)/42)/1,
      ("c"/0).rep(0,5)/42 ==> (""/2 | ("c"/0).rep(1,5)/42)/1,
      ("d"/0).rep(1,3) /42 ==> ("d"/0).rep(1,3) /42,
      (("a"/1 -- "a"/2)/0).? /42 ==> (""/4 | (("a"/1 -- "a"/2)/0))/3,
      (("b"/1).rep /42 | "c"/2)/0 ==> Alternation(Seq((""/4 | ("b"/1).rep(1)/42)/3, "c"/2), 0),
      ("b"/1 | ""/2 | ("c"/3).? /4)/0 ==> Alternation(Seq("b"/1, ""/2, (""/5 | "c"/3)/4), 0)
    )

    forAll(rules) { (rule, expected) =>
      val grammar = modelAssembler.assemble(Seq('S := rule))
      grammar.root shouldEqual expected
    }
  }
}
