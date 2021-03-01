package de.cispa.se.tribble.generation

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.SharedModelAssembler
import de.cispa.se.tribble.{Alternation, Concatenation, GrammarRepr, TestSpecification}

import scala.List
import scala.util.Random

class KPathGeneratorSpec extends TestSpecification with SharedModelAssembler {

  "The GoalBasedTreeGenerator" should "terminate in k-path mode on consecutive references for k>=2" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := "b"
    )
    implicit val grammar: GrammarRepr = modelAssembler.assemble(g.productions)
    implicit val random: Random = new Random(42)

    val regexGenerator = new RegexGenerator(random, 1)
    val shortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, 1)
    val generator = new GoalBasedTreeGenerator(shortestTreeGenerator, new KPathCoverageGoal(2), random)

    val generated = generator.generateForest().take(1000).toList

    generated.size should be < 1000
  }

  "The KPathCoverageGoal" should "have [expr@0->expr@4->expr@4->expr@4 as a target" in {
    val g = Grammar(
      'start := 'expr,
      'expr := "a" | 'expr ~ "+"~'expr
    )
    implicit val grammar: GrammarRepr = modelAssembler.assemble(g.productions)
    implicit val random: Random = new Random(42)

    val goal = new KPathCoverageGoal(4)

    // construct the target path
    val alt = grammar("expr").asInstanceOf[Alternation]
    val concat = alt.alternatives(1).asInstanceOf[Concatenation]
    val start = grammar.start
    val expr4 = concat.elements.head

    goal.targets should contain(List(start, expr4, expr4, expr4))
  }
}
