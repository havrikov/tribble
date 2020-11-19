package de.cispa.se.tribble

import org.scalactic.Equality

/**
  * This mixin provides an equality for DerivationRules
  * which ignores the ids of concatenations, alternations, and quantifications.
  */
trait StructuralDerivationRuleEquality {

  implicit protected object structuralEq extends Equality[DerivationRule] {
    override def areEqual(a: DerivationRule, b: Any): Boolean = a match {
      case Concatenation(elements, _) => b match {
        case Concatenation(other, _) =>
          elements.size == other.size && elements.zip(other).forall(tuple => areEqual(tuple._1, tuple._2))
        case _ => false
      }

      case a: Alternation => b match {
        case b: Alternation =>
          a.alternatives.size == b.alternatives.size && a.alternatives.zip(b.alternatives).forall(tuple => areEqual(tuple._1, tuple._2))
        case _ => false
      }

      case Quantification(subject, min, max, _) => b match {
        case Quantification(other, `min`, `max`, _) => areEqual(subject, other)
        case _ => false
      }

      case _ => a == b
    }
  }

}
