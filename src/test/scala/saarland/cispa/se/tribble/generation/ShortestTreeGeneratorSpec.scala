package saarland.cispa.se.tribble.generation

import org.scalatest.prop.TableDrivenPropertyChecks
import saarland.cispa.se.tribble.TestSpecification
import saarland.cispa.se.tribble.dsl._
import saarland.cispa.se.tribble.input.SharedModelAssembler

import scala.util.Random

class ShortestTreeGeneratorSpec extends TestSpecification with TableDrivenPropertyChecks with SharedModelAssembler {
  private[this] val logger = org.log4s.getLogger

  "The ShortestTreeGenerator" should "respect the close-off choice in alternations" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a" | "a" ~ 'A,
      'B := "b"
    )

    val grammar = modelAssembler.assemble(g.productions)

    val seed: Long = Random.nextInt()
    logger.debug(s"Testing with seed $seed")
    val random: Random = new Random(seed)
    val regexGenerator = new RegexGenerator(random, 1)

    val choices = Table("close-off choice", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    forAll(choices) { choice =>
      val shortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, choice)
      val tree = shortestTreeGenerator.generate(grammar)
      tree.leaves.map(_.value).toList should contain theSameElementsAs List.fill(choice)("a") :+ "b"
    }
  }

  it should "respect the close-off choice in quantifications" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a".rep,
      'B := "b"
    )

    val grammar = modelAssembler.assemble(g.productions)

    val seed: Long = Random.nextInt()
    logger.debug(s"Testing with seed $seed")
    val random: Random = new Random(seed)
    val regexGenerator = new RegexGenerator(random, 1)

    val choices = Table("close-off choice", 2, 3, 4, 5, 6, 7, 8, 9, 10)

    forAll(choices) { choice =>
      val shortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, choice)
      val tree = shortestTreeGenerator.generate(grammar)
      tree.leaves.map(_.value).toList should contain theSameElementsAs List("a", "b")
    }
  }
}
