package de.cispa.se.tribble.generation

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.SharedModelAssembler
import de.cispa.se.tribble.{GrammarRepr, TestSpecification}

import scala.util.Random

class KPathGeneratorSpec extends TestSpecification with SharedModelAssembler {

  "The GoalBasedTreeGenerator" should "terminate in k-path mode on consecutive references for k>=2" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := "b"
    )
    implicit val grammar: GrammarRepr = modelAssembler.assemble(g.productions)
    implicit val reachability: Reachability = new Reachability(grammar)
    implicit val random: Random = new Random(42)

    val regexGenerator = new RegexGenerator(random, 1)
    val shortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, 1)
    val generator = new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, new KPathCoverageGoal(2))

    val generated = generator.generateForest().take(1000).toList

    generated.size should be < 1000
  }
}
