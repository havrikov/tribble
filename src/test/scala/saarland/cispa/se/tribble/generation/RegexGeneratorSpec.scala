package saarland.cispa.se.tribble
package generation

import java.util.regex.Pattern

import dk.brics.automaton.{RegExp, RunAutomaton}

import scala.util.Random

class RegexGeneratorSpec extends TestSpecification {

  "The regex generator" should "generate correct strings" in {
    val regexes = Table(("regex","javaregex"),
      "([\\x20-\\x21]|[\\x23-\\x2B]|[\\x2D-\\x7E])+" -> "[\\x20-\\x21\\x23-\\x2B\\x2D-\\x7E]+",
      "a" -> "a",
      "a?" -> "a?",
      "a+" -> "a+",
      "a*" -> "a*",
      "ab" -> "ab",
      "(a|b)[abc]" -> "(a|b)[abc]",
      "[a-z]+" -> "[a-z]+",
      "[^a-z]+" -> "[^a-z]+",
      "[a-c-]+" -> "[a-c-]+",
      "[-a-c]+" -> "[-a-c]+",
      "[^-a-c]+" -> "[^-a-c]+",
      "[\\^a-c]+" -> "[a-c^]+",
      "[a-c^]+" -> "[a-c^]+",
      "/a" -> "/a",
      "\\/a" -> "/a",
      "\\/\\a" -> "/a",
      "/" -> "/",
      "a\nb" -> "a[\n]b",
      "\n" -> "[\n]",
      "\\\\" -> "\\\\",
      "\\/" -> "/",
      "\\\"" -> "[\"]",
      "\\\\" -> "[\\\\]",
      "a\tb" -> "a[\t]b"
      // todo add tests with & ~ \+ \*
    )

    forAll(regexes) { (regex, pattern) =>
      val automaton = new RegExp(regex).toAutomaton
      val runAutomaton = new RunAutomaton(automaton)
      val seed = Random.nextLong()
      val rnd = new Random(seed)
      val rGenerator = new RegexGenerator(rnd, 0)
      for (i <- 1 to 50) {
        val builder = new StringBuilder
        rGenerator.generateIntoBuilder(automaton, builder)
        val test = builder.mkString
        assert(runAutomaton.run(test), s"Generated string was '$test' at index $i with seed $seed")
        assert(Pattern.matches(pattern, test), s"Generated string was '$test' at index $i with seed $seed")
      }
    }
  }

}
