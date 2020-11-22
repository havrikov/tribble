package de.cispa.se.tribble
package generation

import java.io.File
import java.nio.file.{Files, Paths}

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.{AutomatonCache, ModelAssembler}
import org.log4s.getLogger
import org.scalacheck.Arbitrary
import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.prop.TableFor2
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class NaiveProbabilisticTreeGeneratorSpec extends TestSpecification with ScalaCheckDrivenPropertyChecks {
  private val logger = getLogger
  private val seed = Random.nextLong()
  logger.debug(s"Testing with seed $seed")
  private val random = new Random(seed)
  private val regexGenerator = new RegexGenerator(random, 1)
  private val shortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, 1)
  private val treeGenerator = new NaiveProbabilisticTreeGenerator(10000, regexGenerator, 5, random, shortestTreeGenerator)
  private val automatonDir = Files.createDirectories(Paths.get("automata"))
  private val automatonCache = new AutomatonCache(new File(automatonDir.toString))
  private val modelAssembler = new ModelAssembler(automatonCache, Double.MinPositiveValue, 1.0d)
  private val invertedAssembler = new ModelAssembler(automatonCache, Double.MinPositiveValue, -1.0d)

  "The NaiveProbabilisticTreeGenerator" should "select alternatives according to their probabilities" in {
    val grammars = Table("grammar" -> "expected 'a' count",
      Grammar('S := ("a"@@.5 | "b").rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.9 | "b").rep(1000, 1000)) -> (900 +- 45),
      Grammar('S := ("a"@@.2 | "b").rep(1000, 1000)) -> (200 +- 45),
      Grammar('S := ("a"@@.25 | "b").rep(10000, 10000)) -> (2500 +- 135),
      Grammar('S := ("a"@@.5 | "b"@@.4  | "c").rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.2 | "b"@@.4  | "c").rep(1000, 1000)) -> (200 +- 45),
      Grammar('S := ("a"@@.2 | "b"@@.4  | "c" | "d").rep(1000, 1000)) -> (200 +- 45)
    )

    checkRanges(grammars, modelAssembler)
  }

  it should "correctly scale up probabilities < 1" in {
    val grammars = Table("grammar" -> "expected a count",
      Grammar('S := ("a"@@.25 | "b"@@.25).rep(10000, 10000)) -> (5000 +- 135),
      Grammar('S := ("a"@@.25 | "b"@@.0).rep(10000, 10000)) -> Spread(10000, 0),
      Grammar('S := ("a"@@.1 | "b"@@.1  | "c"@@.1).rep(10000, 10000)) -> (3333 +- 135),
      Grammar('S := ("a"@@.4 | "b"@@.1  | "c"@@.1).rep(10000, 10000)) -> (6666 +- 135),
      Grammar('S := ("a"@@.2 | "b"@@.1  | "c"@@.1).rep(10000, 10000)) -> (5000 +- 135)
    )

    checkRanges(grammars, modelAssembler)
  }

  it should "correctly invert probabilities" in {
    val grammars = Table("grammar" -> "expected a count",
      Grammar('S := ("a"@@.5 | "b").rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.9 | "b").rep(1000, 1000)) -> (100 +- 45),
      Grammar('S := ("a"@@.2 | "b").rep(1000, 1000)) -> (800 +- 45),
      Grammar('S := ("a"@@.25 | "b").rep(10000, 10000)) -> (7500 +- 135),
      Grammar('S := ("a"@@.5 | "b"@@.4  | "c").rep(1000, 1000)) -> (138 +- 45),
      Grammar('S := ("a"@@.2 | "b"@@.4  | "c").rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.1 | "b"@@.4  | "c" | "d").rep(1000, 1000)) -> (488 +- 45)
    )

    checkRanges(grammars, invertedAssembler)
  }

  it should "correctly invert scaled up probabilities" in {
    val grammars = Table("grammar" -> "expected a count",
      Grammar('S := ("a"@@.25 | "b" @@.25).rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.4 | "b" @@.1).rep(1000, 1000)) -> (200 +- 45),
      Grammar('S := ("a"@@.1 | "b"@@.8).rep(1000, 1000)) -> (888 +- 45),
      Grammar('S := ("a"@@.1 | "b"@@.3).rep(10000, 10000)) -> (7500 +- 135),
      Grammar('S := ("a"@@.5 | "b"@@.2  | "c"@@.2).rep(1000, 1000)) -> (167 +- 45)
    )

    checkRanges(grammars, invertedAssembler)
  }

  it should "ignore probabilities when similarity is 0" in {
    val grammars = Table("grammar" -> "expected a count",
      Grammar('S := ("a"@@.25 | "b" @@.25).rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.9 | "b" @@.1).rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.1 | "b"@@.8).rep(1000, 1000)) -> (500 +- 45),
      Grammar('S := ("a"@@.5 | "b"@@.2  | "c"@@.2).rep(1000, 1000)) -> (333 +- 45),
      Grammar('S := ("a"@@.2 | "b"@@.4  | "c" | "d").rep(1000, 1000)) -> (250 +- 45)
    )

    val uniformAssembler = new ModelAssembler(automatonCache, Double.MinPositiveValue, 0.0d)
    checkRanges(grammars, uniformAssembler)
  }

  private def checkRanges(grammars: TableFor2[Grammar, Spread[Int]], assembler: ModelAssembler) = {
    forAll(grammars) { (g, spread) =>
      implicit val grammar: GrammarRepr = assembler.assemble(g.productions)
      implicit def arbTree: Arbitrary[DTree] = Arbitrary(treeGenerator.generate)

      forAll { (t: DTree) =>
        whenever(t.isRoot) {
          val string = t.leaves.mkString
          string.count(Set('a')) shouldBe spread
        }
      }
    }
  }
}
