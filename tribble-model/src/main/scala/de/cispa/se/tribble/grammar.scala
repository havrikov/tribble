package de.cispa.se.tribble

import dk.brics.automaton.Automaton

@SerialVersionUID(6232777775799306102L)
sealed trait DerivationRule extends Serializable {
  /** Offers a non-resolving pre-order walk of this rule. */
  def toStream: Stream[DerivationRule]
  def shortestDerivation: Int
  private[tribble] def shortestDerivation_=(value: Int): Unit
  def probability: Double
  private[tribble] def probability_=(value: Double): Unit
  def children: Iterable[DerivationRule]
}

object DerivationRule {
  implicit def orderingByName[A <: DerivationRule]: Ordering[A] = Ordering.by(_.toString)
}

@SerialVersionUID(-4776596785614533802L)
final case class Reference(name: String, var id: Int = 0) extends DerivationRule {
  override def toStream: Stream[DerivationRule] = Stream(this)
  override def toString: String = s"'$name$id${if(probability.isNaN)""else s"@@$probability"}"
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = Nil
}

@SerialVersionUID(-2583553144102895238L)
final case class Concatenation(elements: Seq[DerivationRule]) extends DerivationRule {
  require(elements.lengthCompare(1) > 0, s"Concatenations must contain more than one element! (Given $elements)")
  override def toString: String = s"${elements.mkString("(", " ~ ", ")")}${if(probability.isNaN)""else s"@@$probability"}"
  override def toStream: Stream[DerivationRule] = this #:: elements.toStream.flatMap(_.toStream)
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = elements
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

@SerialVersionUID(8294090486258532898L)
final case class Alternation(private val alts: Seq[DerivationRule]) extends DerivationRule {
  require(alts.lengthCompare(1) > 0, s"Alternations must contain more than one alternative! (Given $alts)")
  lazy val alternatives: Seq[DerivationRule] = alts.sorted
  override def equals(obj: Any): Boolean = this.canEqual(obj) && alternatives.equals(obj.asInstanceOf[Alternation].alternatives)
  override def toString: String = s"${alternatives.mkString("(", " | ", ")")}${if(probability.isNaN)""else s"@@$probability"}"
  override def toStream: Stream[DerivationRule] = this #:: alternatives.toStream.flatMap(_.toStream)
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = alternatives
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

@SerialVersionUID(6040299837463063224L)
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
    }${if(probability.isNaN)""else s"@@$probability"}"
  }
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = subject :: Nil
}

@SerialVersionUID(-6968053765170830984L)
sealed abstract class TerminalRule extends DerivationRule {
  override def toStream: Stream[DerivationRule] = Stream(this)
  override val shortestDerivation: Int = 0
  override def shortestDerivation_=(value: Int): Unit = throw new IllegalArgumentException("Cannot set shortestDerivation on a TerminalRule!")
  override var probability: Double = Double.NaN
  override val children: Iterable[DerivationRule] = Nil
}

@SerialVersionUID(7282098018897930203L)
final case class Literal(value: String, var id: Int = 0) extends TerminalRule {
  override def toString: String = "\"" + value.flatMap {
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\\' => "\\\\"
    case '"' => "\\\""
    case c => c.toString
  } + s""""$id${if(probability.isNaN)""else s"@@$probability"}"""
}

@SerialVersionUID(-2769895995041560098L)
final case class Regex(value: String, var id: Int = 0) extends TerminalRule {
  private[tribble] var automaton: Automaton = _

  def this(from: Char, to: Char, id: Int) {
    this(s"[$from-$to]", id)
    automaton = Automaton.makeCharRange(from, to)
  }

  // xxx problem with ~ and & outside of "strings"
  override def toString: String = "/" + value.flatMap {
    case '/' => "\\/"
    case c => c.toString
  } + s"/$id${if(probability.isNaN)""else s"@@$probability"}"
}
