package de.cispa.se.tribble
package generation

import java.nio.charset.StandardCharsets

import better.files._
import de.cispa.se.tribble.input.{SharedModelAssembler, TextDSLParser}
import fastparse._
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class SizedTreeGeneratorSpec extends TestSpecification with SharedModelAssembler with ScalaCheckDrivenPropertyChecks with Eventually {
  private[this] val logger = org.log4s.getLogger

  "The SizedTreeGenerator" should "generate trees of approximately requested size" in {
    val seed: Long = Random.nextInt()
    logger.debug(s"Testing with seed $seed")
    implicit val random: Random = new Random(seed)
    val regexGenerator = new RegexGenerator(random, 1)
    val shortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, 1)

    val grammars = Table("path",
      "src/test/resources/json.tribble",
      "src/test/resources/js.tribble",
      "src/test/resources/clojure.tribble",
      "src/test/resources/kotlin.tribble",
      "src/test/resources/regex.tribble",
      "src/test/resources/regex_generative.tribble",
      "src/test/resources/tribble.tribble",
      "src/test/resources/tribble_generative.tribble"
    )

    forAll(grammars) { path =>
      withClue(s"Grammar: $path") {
        val parsed = parse(ParserInput.fromString(File(path).lines(StandardCharsets.UTF_8) mkString "\n"), TextDSLParser.grammar(_))

        eventually(timeout = Timeout(Span(5, Seconds))) {
          inside(parsed) {
            case Parsed.Failure(lastParser, index, extra) => fail(s"Could not parse the grammar file at '$path' expected $lastParser at ${extra.input.prettyIndex(index)}")
            case Parsed.Success(prods, _) =>
              val grammar: GrammarRepr = modelAssembler.assemble(prods)

              forAll((Gen.choose(10, 300), "requested size")) { s: Int =>
                whenever(s >= 10) {
                  val treeGenerator = new SizedTreeGenerator(10, random, shortestTreeGenerator, s, new RandomChoice(random))
                  val tree = treeGenerator.generate(grammar)
                  tree.size() should be >= s
                }
              }
          }
        }
      }
    }
  }
}
