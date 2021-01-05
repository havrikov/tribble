package de.cispa.se.tribble
package model

import java.io.File

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.{EmptyGrammarCache, GrammarLoader, SharedModelAssembler}
import de.cispa.se.tribble.output.KPathEvaluator
import org.scalatest.prop.TableFor1

class KPathEvaluatorSpec extends TestSpecification with SharedModelAssembler {

  private val loader = new GrammarLoader(modelAssembler, EmptyGrammarCache)
  private val grammar: GrammarRepr = loader.loadGrammar(new File("src/test/resources/typesafe/Expr.scala"))
  private val tracker = new ParentTracker(grammar)

  private val evaluator = new KPathEvaluator(tracker)

  "The KPathEvaluator" should "correctly handle 1-paths" in {
    val table = Table(("kPath", "context"),
      Literal("-") -> Reference("term", 4) ~ "-" ~ Reference("expr", 1),
      Literal("+") -> Reference("term", 3) ~ "+" ~ Reference("expr"),
      Literal("-", 1) -> Literal("-", 1) ~ Reference("factor", 4),
      Literal("+", 1) -> Literal("+", 1) ~ Reference("factor", 3),
      Reference("number") -> Reference("number"),
      "[a-z]".regex -> "[a-z]".regex,
      Reference("expr", 3) -> Reference("expr", 3), // here the optional "\n".? is irrelevant
      Literal("\n") -> Reference("expr", 3) ~ "\n".? // here it is not
    )

    forAll(table) { (kPath, context) =>
      evaluator.evaluateKPath(List(kPath)) shouldEqual context
    }
  }

  it should "correctly handle k-paths" in {
    val table = Table(("kPath", "context"),
      // [expr@3->"-"
      List(Reference("expr", 3), Literal("-")) -> Reference("term", 4) ~ "-" ~ Reference("expr", 1),
      // [expr@3->term@2->factor
      List(Reference("expr", 3), Reference("term", 2), Reference("factor")) -> Reference("factor"),
      // [factor@2->expr@2->"-"
      List(Reference("factor", 2), Reference("expr", 2), Literal("-")) -> Reference("term", 1) ~ "/" ~ "(" ~ Reference("term", 4) ~ "-" ~ Reference("expr", 1) ~ ")"
    )
    forAll(table) { (kPath, context) =>
      evaluator.evaluateKPath(kPath) shouldEqual context
    }
  }


  it should "handle impossible k-paths appropriately" ignore {
    val table: TableFor1[List[DerivationRule]] = Table("kPath",
      // ["-"->expr@3
      List(Literal("-"), Reference("expr", 3)),
      // [number->variable
      List(Reference("number"), Reference("variable")),
    )

    forAll(table) { kPath =>
      assertThrows[IllegalArgumentException] {
        evaluator.evaluateKPath(kPath)
      }
    }
  }

  it should "throw for nonexistent elements" in {
    val table: TableFor1[List[DerivationRule]] = Table("kPath", List("1"), List(Literal("-", 3)), List('literal, 'something), List(Regex("[a-f]")))

    forAll(table) { kPath =>
      assertThrows[NoSuchElementException] {
        evaluator.evaluateKPath(kPath)
      }
    }
  }

}
