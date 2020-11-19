package de.cispa.se.tribble
package model

import java.io.File

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.TestDSL._
import de.cispa.se.tribble.input.{EmptyGrammarCache, GrammarLoader, SharedModelAssembler}
import de.cispa.se.tribble.output.KPathEvaluator
import org.scalatest.prop.{TableFor1, TableFor2}

class KPathEvaluatorSpec extends TestSpecification with SharedModelAssembler with StructuralDerivationRuleEquality {

  private val loader = new GrammarLoader(modelAssembler, EmptyGrammarCache)
  private val grammar: GrammarRepr = loader.loadGrammar(new File("src/test/resources/typesafe/Expr.scala"))
  private val tracker = new ParentTracker(grammar)

  private val evaluator = new KPathEvaluator(tracker)

  "The KPathEvaluator" should "correctly handle 1-paths" in {
    val table: TableFor2[DerivationRule, DerivationRule] = Table(("kPath", "context"),
      "-"/19 ==>'term/18 -- "-"/19 -- 'expr/20,
      "+"/15 ==> 'term/14 -- "+"/15 -- 'expr/16,
      "-"/31 ==> "-"/31 -- 'factor/32,
      "+"/28 ==> "+"/28 -- 'factor/29,
      'number/35 ==> 'number/35,
      "[a-z]".regex/33 ==> "[a-z]".regex/33,
      'expr/38 ==> 'expr/38, // here the optional "\n".? is irrelevant
      "\n"/40 ==> 'expr/38 -- ("\n"/40).? // here it is not
    )

    forAll(table) { (kPath, context) =>
      evaluator.evaluateKPath(List(kPath)) shouldEqual context
    }
  }

  it should "correctly handle k-paths" in {
    val table = Table(("kPath", "context"),
      List('expr/38, "-"/19) ==> 'term/18 -- "-"/19 -- 'expr/20,
      List('expr/38, 'term/12, 'factor/2) ==> 'factor/2,
      List('factor/10, 'expr/25, "-"/19) ==> 'term/8 -- "/"/9 -- "("/24 -- 'term/18 -- "-"/19 -- 'expr/20 -- ")"/26
    )
    forAll(table) { (kPath, context) =>
      evaluator.evaluateKPath(kPath) shouldEqual context
    }
  }


  it should "handle impossible k-paths appropriately" ignore {
    val table: TableFor1[List[DerivationRule]] = Table("kPath",
      List("-"/19, 'expr/20),
      List('number/35, 'variable/36),
    )

    forAll(table) { kPath =>
      assertThrows[IllegalArgumentException] {
        evaluator.evaluateKPath(kPath)
      }
    }
  }

  it should "throw for nonexistent elements" in {
    val table: TableFor1[List[DerivationRule]] = Table("kPath", List("1"/42), List("-"/3), List('literal/22, 'something/0), List("[a-f]".regex))

    forAll(table) { kPath =>
      assertThrows[NoSuchElementException] {
        evaluator.evaluateKPath(kPath)
      }
    }
  }

}
