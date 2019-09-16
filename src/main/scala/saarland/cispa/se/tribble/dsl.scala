package saarland.cispa.se.tribble

import saarland.cispa.se.tribble.model._

import scala.language.implicitConversions

object dsl {

  final case class Grammar(productions: Production*) {
    require(productions.nonEmpty, "The grammar must have productions!")
  }

  implicit def literal(value: String): Literal = Literal(value)

  implicit def nonTerminal(nonTerminal: Symbol): Reference = Reference(nonTerminal.name)

  implicit class RichChar(val c: Char) extends AnyVal {
    def ->(to: Char): Regex = new Regex(c, to, 0)
  }

  implicit class RegexString(val value: String) extends AnyVal {
    def regex: Regex = Regex(value, null)
  }

  implicit class RichNonTerminal(val ref: Symbol) extends AnyVal {
    def :=[T](rhs: T)(implicit conv: T => DerivationRule): Production = ref.name -> rhs
  }

  implicit class RichDerivationRule[E](val rule: E)(implicit converter: E => DerivationRule) {
    def ? : Quantification = Quantification(rule, 0, 1)

    def rep: Quantification = rep()

    def rep(min: Int = 0, max: Int = Int.MaxValue): Quantification = {
      require(min >= 0, s"Minimum repetition must be zero or more! ($min given)")
      require(min <= max, "Minimum repetition must not be greater than maximum!")
      Quantification(rule, min, max)
    }

    def |[T](rhs: T)(implicit conv: T => DerivationRule): Alternation = Alternation(alternate(rule) ++ alternate(rhs))

    def ~[T](rhs: T)(implicit conv: T => DerivationRule): Concatenation = Concatenation(concatenate(rule) ++ concatenate(rhs))

    private def alternate(rhs: DerivationRule): Set[DerivationRule] = rhs match {
      case Alternation(alternatives) => alternatives
      case _ => Set(rhs)
    }

    private def concatenate(rhs: DerivationRule): Seq[DerivationRule] = rhs match {
      case Concatenation(elements) => elements
      case _ => Seq(rhs)
    }
  }

}
