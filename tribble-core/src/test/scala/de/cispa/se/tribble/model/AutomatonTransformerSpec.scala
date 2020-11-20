package de.cispa.se.tribble
package model

import better.files._
import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.{ModelAssembler, ParseGrammar, SharedAutomatonCache}
import dk.brics.automaton.RegExp
import org.scalatest.prop.TableFor2

class AutomatonTransformerSpec extends TestSpecification with SharedAutomatonCache {

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

  it should "transform simple alternatives correctly" ignore {
    val grammars: TableFor2[NonTerminal, List[DerivationRule]] = Table(("regex", "expected"),
      "a(b|c)" -> List("a" ~ 'r1, "[b-c]".regex),
      "a[bc]" -> List("a" ~ 'r1, "[b-c]".regex),
      "a[b-c]" -> List("a" ~ 'r1, "[b-c]".regex),
      "a(b|c)" -> List("a" ~ 'r1, "[b-c]".regex),
      "ab|c" -> List("a" ~ 'r1 | "c", "b"),
    )

    forAll(grammars) { (regex, expected) => transformed(regex).values should contain allElementsOf expected }
  }

  it should "transform character ranges correctly" ignore {
    transformed("[a-z]").values should contain ("[a-z]".regex)
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

  it should "leave ids of newly created rules unassigned" in {
    val content =
      """
        |Grammar(
        |'S := "[^a]*".regex
        |)
        |""".stripMargin

    val file = "build" / "tmp" / "test" / "regex_id_test.scala"
    file.createFileIfNotExists(createParents = true)
      .overwrite(content)

    val modelAssembler = new ModelAssembler(automatonCache, Double.MinPositiveValue, 1.0d, transformRegexes = true, checkIds = true)
    // this must not throw
    ParseGrammar(modelAssembler).load(file.toJava)
  }

}
