package saarland.cispa.se.tribble

import org.scalactic.Equality
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Inside, Matchers}
import saarland.cispa.se.tribble.model.{DerivationRule, Regex}

class TestSpecification extends FlatSpec with Matchers with TableDrivenPropertyChecks with Inside {

  implicit protected object regexEq extends Equality[DerivationRule] {
    override def areEqual(a: DerivationRule, b: Any): Boolean = a match {
      case Regex(leftPattern, _, _) => b match {
        case Regex(pattern, _, _) => leftPattern == pattern
        case _ => a == b
      }
      case _ => a == b
    }
  }
}
