package de.cispa.se.tribble
package input

import fastparse._
import fastparse.ScalaWhitespace._

private[tribble] object TextDSLParser extends InputGrammarParser {
  private def escape[_: P]: P[String] = P("\\" ~~ CharPred("\"\\nrt".contains(_))).!.map(StringContext.treatEscapes)

  private def strChars[_: P]: P[String] = CharsWhile(!"\"\\".contains(_)).!

  private def terminal[_: P]: P[Literal] = "\"" ~~ (strChars | escape).repX.map(_.mkString).map(Literal(_)) ~~ "\""

  private def regexEscape[_: P]: P[String] = escape | ("\\" ~~ CharPred("/()".contains(_)).!)

  private def regex[_: P]: P[Regex] = P("/" ~~ !"/" ~~ P(regexEscape | CharsWhile(!"\"/\\".contains(_)).!).repX ~~ "/")
    .map(_.mkString)
    .map(Regex(_))

  private def reference[_: P]: P[Reference] = CharsWhile((('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ "_$'").toSet.contains(_)).!.map(Reference(_))

  private def num[_: P]: P[Int] = CharsWhileIn("0-9").!.map(_.toInt)

  private def repQuant[_: P]: P[(Int, Int)] = P(num ~ &("}")).filter(_ > 1).map(n => (n, n))

  private def oneQuant[_: P]: P[(Int, Int)] = P("," ~ num).map(n => (0, n)) | P(num ~ "," ~ &("}")).map(n => (n, Int.MaxValue))

  private def twoQuant[_: P]: P[(Int, Int)] = P(num ~ "," ~ num).filter { case (min, max) => min <= max && (max != 1 || min != 1) }

  private def braceQuant[_: P]: P[(Int, Int)] = P("{" ~/ (repQuant | oneQuant | twoQuant) ~ "}")

  private def kleeneQuant[_: P]: P[(Int, Int)] = P("?".!.map(_ => (0, 1))
    | "*".!.map(_ => (0, Int.MaxValue))
    | "+".!.map(_ => (1, Int.MaxValue)))

  private def quantifier[_: P]: P[(Int, Int)] = P(braceQuant | kleeneQuant)

  private def atom[_: P]: P[DerivationRule] = P(("(" ~/ alternation ~ ")" | regex | terminal | reference) ~ quantifier.?).map {
    case (sym, None) => sym
    case (sym, Some((min, max))) => Quantification(sym, min, max)
  }

  private def prob[_: P]: P[Double] = "@@" ~/ CharsWhile("0123456789.xXabcdefABCDEFpP-".contains(_)).!.map(_.toDouble)

  private def concatenation[_: P]: P[DerivationRule] = (atom.rep(1).map { case Seq(e) => e case seq => Concatenation(seq) } ~ prob.?).map {
    case (c, None) => c
    case (c, Some(p)) =>
      c.probability = p
      c
  }

  private def alternation[_: P]: P[DerivationRule] = concatenation.rep(min = 1, sep = "|"./).map(_.distinct).map { case Seq(e) => e case seq => Alternation(seq) }

  private def production[_: P]: P[Production] = P(reference ~ "=" ~/ alternation ~ ";").map { case (Reference(name, _), rhs) => name -> rhs }

  override def grammar[_: P]: P[Seq[Production]] = P(Start ~ production.rep(1) ~ End)
}
