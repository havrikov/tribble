package de.cispa.se.tribble
package generation

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.SharedModelAssembler
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class MaxDepthGeneratorSpec extends TestSpecification with ScalaCheckDrivenPropertyChecks with SharedModelAssembler {
  private[this] val logger = org.log4s.getLogger

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 1000)

  "MaxDepthGenerator" should "respect the maximum depth" in {
    val seed = Random.nextInt()
    logger.debug(s"Testing with seed $seed")
    implicit val random: Random = new Random(seed)
    val regexGenerator = new RegexGenerator(random, 1)
    val randomChoice = new RandomChoice(random)

    /*
    0     ~
         / \
    1   1   C
            |
    2      alt
          / |  \
    3    a  ~   E
           / \   \
    4     b   b  alt
                 / \
    5           ~   g
               / \
    6         e   f
    */
    val g = Grammar(
      'S := "1" ~ 'C,
      'C := "a" | "b" ~ "b" | 'E,
      'E := "e" ~ "f" | "g"
    )
    implicit val grammar: GrammarRepr = modelAssembler.assemble(g.productions)

    // cannot produce trees of depth 2 or less
    an[IllegalArgumentException] should be thrownBy {
      new MaxDepthGenerator(10, random, regexGenerator, 2, randomChoice).generate
    }

    // can produce trees of depth 3 and more
    val depths = Table("depth", 3, 4, 5, 6, 7)
    forAll(depths) { maxDepth =>
      val generator = new MaxDepthGenerator(10, random, regexGenerator, maxDepth, randomChoice)

      implicit def arbTree: Arbitrary[DTree] = Arbitrary(generator.generate)

      forAll("tree") { t: DTree =>
        t.depth() should be <= maxDepth
      }
    }
  }

}
