package de.cispa.se.tribble

import de.cispa.se.tribble.DerivationRule.DEFAULT_ID
import dk.brics.automaton.Automaton

@SerialVersionUID(6232777775799306103L)
sealed trait DerivationRule extends Serializable {
  /** Offers a non-resolving pre-order walk of this rule. */
  def toStream: Stream[DerivationRule]
  def shortestDerivation: Int
  private[tribble] def shortestDerivation_=(value: Int): Unit
  def probability: Double
  private[tribble] def probability_=(value: Double): Unit
  def id: Int
  private[tribble] def id_=(value: Int): Unit
  def children: Iterable[DerivationRule]
}

object DerivationRule {
  implicit def orderingByName[A <: DerivationRule]: Ordering[A] = Ordering.by(_.toString)
  /** If a derivation rule has this as the value of id, it was probably not properly initialized. */
  val DEFAULT_ID: Int = Int.MinValue
}

/*
The declarations below have an explicitly defined secondary constructor,
which is identical with leaving out the optional parameter of the primary constructor.
This is only needed to enable smooth interoperability with consumers from Java and Kotlin.
 */

@SerialVersionUID(-4776596785614533803L)
final case class Reference(name: String, override var id: Int = DEFAULT_ID) extends DerivationRule {
  private[tribble] def this(name: String) = this(name, DEFAULT_ID)
  override def toStream: Stream[DerivationRule] = Stream(this)
  override def toString: String = s"'$name$id${if(probability.isNaN)""else s"@@$probability"}"
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = Nil
}

@SerialVersionUID(-2583553144102895239L)
final case class Concatenation(elements: Seq[DerivationRule], override var id: Int = DEFAULT_ID) extends DerivationRule {
  require(elements.size > 1, s"Concatenations must contain more than one element! (Given $elements)")
  private[tribble] def this(elements: Seq[DerivationRule]) = this(elements, DEFAULT_ID)
  override def toString: String = s"${elements.mkString("(", " ~ ", s")c$id")}${if(probability.isNaN)""else s"@@$probability"}"
  override def toStream: Stream[DerivationRule] = this #:: elements.toStream.flatMap(_.toStream)
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = elements
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

@SerialVersionUID(8294090486258532899L)
final case class Alternation(private val alts: Seq[DerivationRule], override var id: Int = DEFAULT_ID) extends DerivationRule {
  private[tribble] def this(alts: Seq[DerivationRule]) = this(alts, DEFAULT_ID)
  lazy val alternatives: Seq[DerivationRule] = alts.sorted.view.force
  override def equals(obj: Any): Boolean = this.canEqual(obj) && alternatives.equals(obj.asInstanceOf[Alternation].alternatives)
  override def toString: String = s"${alternatives.mkString("(", " | ", s")a$id")}${if(probability.isNaN)""else s"@@$probability"}"
  override def toStream: Stream[DerivationRule] = this #:: alternatives.toStream.flatMap(_.toStream)
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = alternatives
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

@SerialVersionUID(6040299837463063225L)
final case class Quantification(subject: DerivationRule, min: Int, max: Int, override var id: Int = DEFAULT_ID) extends DerivationRule {
  require(min >= 0, s"Quantifications must have a non-negative minimum! (Given $min)")
  require(max >= min, s"Quantifications must have a valid range! (Given $min to $max)")
  require(min != 0 || max != 0, "Quantifications must not have both minimum and maximum of zero!")
  private[tribble] def this(subject: DerivationRule, min: Int, max: Int) = this(subject, min, max, DEFAULT_ID)
  override def toStream: Stream[DerivationRule] = this #:: subject.toStream
  override def toString: String = {
    s"($subject)${(min, max) match {
        case (0, 1) => "?"
        case (0, Int.MaxValue) => "*"
        case (1, Int.MaxValue) => "+"
        case _ => s"{$min,$max}"
      }
    }q$id${if(probability.isNaN)""else s"@@$probability"}"
  }
  override var shortestDerivation: Int = Int.MaxValue
  override var probability: Double = Double.NaN
  override def children: Iterable[DerivationRule] = subject :: Nil
}

@SerialVersionUID(-6968053765170830985L)
sealed abstract class TerminalRule extends DerivationRule {
  override def toStream: Stream[DerivationRule] = Stream(this)
  override val shortestDerivation: Int = 0
  override def shortestDerivation_=(value: Int): Unit = throw new IllegalArgumentException("Cannot set shortestDerivation on a TerminalRule!")
  override var probability: Double = Double.NaN
  override val children: Iterable[DerivationRule] = Nil
}

@SerialVersionUID(7282098018897930204L)
final case class Literal(value: String, override var id: Int = DEFAULT_ID) extends TerminalRule {
  private[tribble] def this(value: String) = this(value, DEFAULT_ID)
  override def toString: String = "\"" + value.flatMap {
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\\' => "\\\\"
    case '"' => "\\\""
    case c => c.toString
  } + s""""$id${if(probability.isNaN)""else s"@@$probability"}"""
}

@SerialVersionUID(-2769895995041560099L)
final case class Regex(value: String, override var id: Int = DEFAULT_ID) extends TerminalRule {
  private[tribble] def this(value: String) = this(value, DEFAULT_ID)
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
