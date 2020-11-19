package de.cispa.se.tribble

import de.cispa.se.tribble.dsl._

/*
Scala's operator priority

(characters not shown below)
* / %
+ -
:
= !
< >
&
^
|
(all letters)
 */

object TestDSL {

  implicit final class EvenRicherDerivationRule[E](private val rule: E)(implicit converter: E => DerivationRule) {

    /** Assign this rule the given id. */
    def /(id: Int): DerivationRule = {
      // the type ascription forces the implicit conversion from E to DerivationRule before assigning the id
      val r: DerivationRule = rule
      r.id = id
      r
    }

    /** Alternative way to write down a concatenation. */
    def --[T](rhs: T)(implicit conv: T => DerivationRule): Concatenation = rule ~ rhs

  }

  implicit final class AlternativePair[A](private val self: A) extends AnyVal {
    /** Alternative way to write down a pair. */
    @inline def ==>[B](y: B): (A, B) = Tuple2(self, y)
  }

}
