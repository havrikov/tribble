package saarland.cispa.se.tribble.model

import dk.brics.automaton.RegExp
import org.scalatest.prop.TableFor2
import saarland.cispa.se.tribble.dsl._
import saarland.cispa.se.tribble.{NonTerminal, TestSpecification}

class AutomatonTransformerSpec extends TestSpecification {

  private def transformed(regex: String) = {
    val automaton = new RegExp(regex, RegExp.COMPLEMENT | RegExp.INTERSECTION).toAutomaton
    val (terminal, productions) = AutomatonTransformer.transform(automaton, "r")
    terminal shouldEqual "r0"
    productions
  }

  "The AutomatonTransformer" should "transform simple sequences correctly" in {
    val grammars = Table(("regex", "expected"),
      "a" -> Map('r0 := "a"),
      "ab" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "a(b)" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "(a)b" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "(a)(b)" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "(ab)" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "(a(b))" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "((a)b)" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      "((a)(b))" -> Map('r0 := "a" ~ 'r1, 'r1 := "b"),
      // testing bigger expressions is flaky because of randomly assigned indices
      // "abc" -> Map('r0 := "a" ~ 'r1, 'r1 := "b" ~ 'r2, 'r2 := "c"),
    )

    forAll(grammars) { (regex, expected) => transformed(regex) shouldEqual expected }
  }

  it should "transform operators correctly" in {
    val grammars = Table(("regex", "expected"),
      "ab?" -> Map('r0 := "a" ~ 'r1 | "a", 'r1 := "b"),
      "a?" -> Map('r0 := "" | "a"), // special case when start state is accepting
      "a+" -> Map('r0 := "a" ~ 'r1 | "a", 'r1 := "a" ~ 'r1 | "a"),
      "a*" -> Map('r0 := "a" | "a" ~ 'r0 | ""),
    )

    forAll(grammars) { (regex, expected) => transformed(regex) shouldEqual expected }
  }

}
