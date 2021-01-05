package de.cispa.se.tribble

import org.scalactic.Equality
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class TestSpecification extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with Inside {

  // TODO check if this is still needed after the Regex check
  implicit protected object regexEq extends Equality[DerivationRule] {
    override def areEqual(a: DerivationRule, b: Any): Boolean = a match {
      case Regex(leftPattern, _) => b match {
        case Regex(pattern, _) => leftPattern == pattern
        case _ => a == b
      }
      case _ => a == b
    }
  }
}
