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

    val goal = new KPathCoverageGoal(k = 4)(grammar, random, new Reachability(grammar))

    goal.targets should contain(List('expr/10, 'expr/20, 'expr/20, 'expr/20))
  }

  it should "build recursive loops" in {
    val g = Grammar(
      'S := 'A/10,
      'A := "a" | 'A/42
    )

    val grammar = modelAssembler.assemble(g.productions)
    val goal = new KPathCoverageGoal(k = 5)(grammar, new Random(42), new Reachability(grammar))

    goal.targets should not be empty
  }

  it should "include the root as a 1-path target if it is a reference" in {
    val g = Grammar(
      'S := 'A/1,
      'A := "a"/2
    )

    val grammar = modelAssembler.assemble(g.productions)
    val reach = new Reachability(grammar)
    val goal = new KPathCoverageGoal(k = 1)(grammar, new Random(42), reach)

    reach.interestingRules should contain theSameElementsAs Set('A/1, "a"/2)
    goal.targets should contain theSameElementsAs Set(List('A/1), List("a"/2))
  }
}
