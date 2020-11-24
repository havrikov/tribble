package de.cispa.se.tribble
package input

import de.cispa.se.tribble.dsl._

class RuleInliningSpec extends TestSpecification with SharedModelAssembler {

  "The RuleInlining" should "inline references correctly" in {
    val g = Grammar(
      'A := "a" ~ 'B ~ 'C,
      'B := "b" ~ 'C,
      'C := "c"
    )
    val grammar = modelAssembler.assemble(g.productions)
    val inlinedOnce = new RuleInlining(1).process(grammar)
    inlinedOnce.rules should have size 2

    val inlinedTwice = new RuleInlining(2).process(grammar)
    inlinedTwice.rules should have size 1
  }

  it should "be able to inline references multiple times" in {
    val g = Grammar(
      'S := 'A,
      'A := "" | "a" ~ 'A
    )
    val grammar = modelAssembler.assemble(g.productions)
    for (repetitions <- 1 to 7) {
      val inlining = new RuleInlining(repetitions)
      val inlined = inlining.process(grammar)
      inlined("A").toStream.length shouldEqual math.pow(2, repetitions + 2).intValue + 1
    }
  }
}
