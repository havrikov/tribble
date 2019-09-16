package saarland.cispa.se.tribble
package model

import dk.brics.automaton.Automaton

sealed trait DerivationRule {
  /** offers a non-resolving pre-order walk of this rule */
  def toStream: Stream[DerivationRule] = this #:: Stream.empty[DerivationRule]
  def shortestDerivation: Int
  def children: Iterable[DerivationRule]
}

final case class Reference(name: NonTerminal, var id: Int = 0) extends DerivationRule {
  override def toString: String = s"'$name$id"
  var shortestDerivation: Int = Int.MaxValue
  override def children: Iterable[DerivationRule] = Nil
}

final case class Concatenation(elements: Seq[DerivationRule]) extends DerivationRule {
  require(elements.lengthCompare(1) > 0, s"Concatenations must contain more than one element! (Given $elements)")
  override def toString: String = s"${elements.mkString("(", " ~ ", ")")}"
  override def toStream: Stream[DerivationRule] = this #:: elements.toStream.flatMap(_.toStream)
  var shortestDerivation: Int = Int.MaxValue
  override def children: Iterable[DerivationRule] = elements
}

final case class Alternation(alternatives: Set[DerivationRule]) extends DerivationRule {
  require(alternatives.size > 1, s"Alternations must contain more than one alternative! (Given $alternatives)")
  override def toString: String = s"${alternatives.mkString("(", " | ", ")")}"
  override def toStream: Stream[DerivationRule] = this #:: alternatives.toStream.flatMap(_.toStream)
  var shortestDerivation: Int = Int.MaxValue
  override def children: Iterable[DerivationRule] = alternatives
}

final case class Quantification(subject: DerivationRule, min: Int, max: Int) extends DerivationRule {
  require(min >= 0, s"Quantifications must have a non-negative minimum! (Given $min)")
  require(max >= min, s"Quantifications must have a valid range! (Given $min to $max)")
  override def toStream: Stream[DerivationRule] = this #:: subject.toStream
  override def toString: String = {
    s"($subject)${(min, max) match {
        case (0, 1) => "?"
        case (0, Int.MaxValue) => "*"
        case (1, Int.MaxValue) => "+"
        case _ => s"{$min,$max}"
      }
    }"
  }
  var shortestDerivation: Int = Int.MaxValue
  override def children: Iterable[DerivationRule] = subject :: Nil
}

sealed abstract class TerminalRule extends DerivationRule {
  override val shortestDerivation: Int = 0
  override val children: Iterable[DerivationRule] = Nil
}

final case class Literal(value: String, var id: Int = 0) extends TerminalRule {
  override def toString: String = "\"" + value.flatMap {
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\\' => "\\\\"
    case '"' => "\\\""
    case c => c.toString
  } + s""""$id"""
}

final case class Regex(value: String, var automaton: Automaton, var id: Int = 0) extends TerminalRule {
  def this(from: Char, to: Char, id: Int) {
    this(s"[$from-$to]", Automaton.makeCharRange(from, to), id)
  }

  // xxx problem with ~ and & outside of "strings"
  override def toString: String = "/" + value.flatMap {
    case '/' => "\\/"
    case c => c.toString
  } + s"/$id"
}
