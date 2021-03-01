package de.cispa.se.tribble
package generation

import de.cispa.se.tribble.TestDSL._
import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.SharedModelAssembler

import scala.util.Random

class KPathCoverageGoalSpec extends TestSpecification with SharedModelAssembler {

  "The KPathCoverageGoal" should "have all 4-paths as targets" in {
    val g = Grammar(
      'start := 'expr/10,
      'expr := "a" | 'expr/20 -- "+" -- 'expr/30
    )
    val grammar = modelAssembler.assemble(g.productions)
    val random = new Random(42)

    val goal = new KPathCoverageGoal(k = 4)(grammar, random)

    goal.targets should contain(List('expr/10, 'expr/20, 'expr/20, 'expr/20))
  }
}
