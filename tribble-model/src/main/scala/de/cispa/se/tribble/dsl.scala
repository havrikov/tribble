package de.cispa.se.tribble

import scala.language.implicitConversions

object dsl {
  type Production = (String, DerivationRule)
  /**
    * The entry point into defining your grammar.
    * <p>
    * For example:
    * <pre>
    * Grammar(
    * 'S := 'A | 'B,
    * 'A := "a",
    * 'B := "b" ~ ('C | 'D),
    * 'C := "c".rep(2,5),
    * 'D := "[a-z]+".regex
    * )
    * </pre>
    */
  final case class Grammar(productions: Production*) {
    require(productions.nonEmpty, "A grammar must have productions!")
  }

  /** Automatically translates a literal string into a [[Literal]] grammar object. */
  implicit def literal(value: String): Literal = Literal(value)

  /** Automatically translates a [[Symbol]] into a [[Reference]] grammar object.  */
  implicit def nonTerminal(nonTerminal: Symbol): Reference = Reference(nonTerminal.name)

  implicit final class RichChar(private val start: Char) extends AnyVal {
    /** Create a [[Regex]] range from [[start]] to `stop` (inclusive). */
    def ->(stop: Char): Regex = new Regex(start, stop, 0)
  }

  implicit final class RegexString(private val value: String) extends AnyVal {
    /** Create a [[Regex]] grammar object from a literal string as defined by
      * https://www.brics.dk/automaton/doc/index.html?dk/brics/automaton/RegExp.html
      */
    def regex: Regex = Regex(value)
  }

  implicit final class RichNonTerminal[R](private val ref: R)(implicit converter: R => Reference) {
    /** Create a production rule mapping the `NonTerminal` on the left side to the [[DerivationRule]] on the right. */
    def :=[T](rhs: T)(implicit conv: T => DerivationRule): Production = ref.name -> rhs
  }

  implicit final class RichDerivationRule[E](private val rule: E)(implicit converter: E => DerivationRule) {
    /** Mark this rule as optional. */
    def ? : Quantification = Quantification(rule, 0, 1)

    /** Mark this rule as repeatable zero or more times. */
    def rep: Quantification = rep()

    /** Mark this rule as repeatable `min` to `max` times (inclusive). */
    def rep(min: Int = 0, max: Int = Int.MaxValue): Quantification = Quantification(rule, min, max)

    def @@[T](prob: T)(implicit conv: T => Double): DerivationRule = {
      require(prob >= 0.0d, s"The probability must be positive! ($prob given)")
      require(prob <= 1.0d, s"The probability must be less than or equal to one! ($prob given)")
      // the type ascription forces the implicit conversion from E to DerivationRule before assigning the probability
      val r: DerivationRule = rule
      r.probability = prob
      r
    }

    /** Create a [[DerivationRule]] which can derive either into the left or the right argument. */
    def |[T](rhs: T)(implicit conv: T => DerivationRule): Alternation = Alternation(alternate(rule) ++ alternate(rhs))

    /** Create a [[DerivationRule]] which must derive into the left argument followed by the right. */
    def ~[T](rhs: T)(implicit conv: T => DerivationRule): Concatenation = Concatenation(concatenate(rule) ++ concatenate(rhs))

    /** Flatten an alternation "(a | b) | c" into "a | b | c" if needed. */
    private def alternate(rhs: DerivationRule): Seq[DerivationRule] = rhs match {
      case a: Alternation => a.alternatives
      case _ => Seq(rhs)
    }

    /** Flatten a concatenation "(a ~ b) ~ c" into "a ~ b ~ c" if needed. */
    private def concatenate(rhs: DerivationRule): Seq[DerivationRule] = rhs match {
      case c: Concatenation => c.elements
      case _ => Seq(rhs)
    }
  }

}
