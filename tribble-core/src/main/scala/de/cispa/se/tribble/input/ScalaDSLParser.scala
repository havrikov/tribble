package de.cispa.se.tribble
package input

import fastparse._

/** Parses the Scala DSL grammar format. */
private[tribble] object ScalaDSLParser extends InputGrammarParser {
  import fastparse.ScalaWhitespace._

  private def stringBased[_:P]: P[TerminalRule] = P(ScalaStringParser.string ~ P("." ~ "regex").!.?).map {
    case (str, None) => Literal(str)
    case (str, _) => Regex(str)
  }

  // TODO needed for the js grammar
  //  private def charRange[_:P]: P[Regex] = P("'" ~~ ??? ~ "->" ~ "'")

  private def reference[_: P]: P[Reference] = P("'" ~~ CharsWhileIn("_$a-zA-Z0-9").!.map(Reference(_)))

  private def num[_: P]: P[Int] = P(CharsWhileIn("0-9").!.map(_.toInt))

  private def oneQuant[_: P]: P[(Int, Int)] = P(num.map(n => (n, Int.MaxValue)))

  private def twoQuant[_: P]: P[(Int, Int)] = P(num ~ "," ~ num).filter { case (min, max) => min <= max && (max != 1 || min != 1) }

  private def repQuant[_: P]: P[(Int, Int)] = P("rep" ~/ ("(" ~ (twoQuant | oneQuant) ~ ")" | Pass(0 -> Int.MaxValue)))

  private def kleeneQuant[_: P]: P[(Int, Int)] = P("?" ~/ Pass(0 -> 1))

  private def quantifier[_: P]: P[(Int, Int)] = P("." ~/ (kleeneQuant | repQuant))

  private def atom[_: P]: P[DerivationRule] = P(("(" ~/ alternation ~ ")" | reference | stringBased) ~ quantifier.?).map {
    case (sym, None) => sym
    case (sym, Some((min, max))) => Quantification(sym, min, max)
  }

  private def prob[_: P]: P[Double] = "@@" ~/ CharsWhileIn("pP.xX0-9a-fA-F\\-").!.map(_.toDouble)

  private def concatenation[_: P]: P[DerivationRule] = P(atom.rep(1, sep="~"./).map { case Seq(e) => e case seq => Concatenation(seq) } ~ prob.?).map {
    case (c, None) => c
    case (c, Some(p)) =>
      c.probability = p
      c
  }

  private def alternation[_: P]: P[DerivationRule] = P(concatenation.rep(min = 1, sep = "|"./)).map(_.distinct).map { case Seq(e) => e case seq => Alternation(seq) }

  private def production[_: P]: P[Production] = P(reference ~ ":=" ~/ alternation).map { case (Reference(name, _), rhs) => name -> rhs }

  private def importStatement[_: P] = P("import" ~/ "de.cispa.se.tribble.dsl._" ~/ ";".?)

  private def constructorStatement[_: P] = P("Grammar" ~/ "(" ~ production.rep(1, sep = ",") ~ ")")

  override def grammar[_: P]: P[Seq[Production]] = P(Start ~ importStatement.? ~ constructorStatement ~ End)
}

private object ScalaStringParser {
  import fastparse.NoWhitespace._

  private val unicodeRegex = """\\u+([0-9a-fA-F]{4})""".r

  // taken from https://stackoverflow.com/a/29686933
  private def unescapeUnicode(str: String): String =
    unicodeRegex.replaceAllIn(str,
      m => Integer.parseInt(m.group(1), 16).toChar match {
        case '\\' => """\\"""
        case '$' => """\$"""
        case c => c.toString
      })

  private def tqStrChars[_:P]: P[String] = P(CharsWhile(_ != '"') | "\"" ~ "\"".? ~ !"\"").!

  private def tqString[_:P]: P[String] = P("\"\"\"" ~/ tqStrChars.repX.map(_.mkString) ~ "\"\"\"" ~ "\"".rep.!).map {case (main, tail) =>  unescapeUnicode(main + tail)}

  private def escape[_:P]: P[String] = P("\\" ~/ AnyChar).!

  private def sqStrChar[_:P]: P[String] = P(CharsWhile(!"\"\n\\".contains(_))).!

  private def sqString[_:P]: P[String] = P("\"" ~/ (sqStrChar | escape).repX.map(_.mkString).map(unescapeUnicode).map(StringContext.treatEscapes) ~ "\"")

  def string[_:P]: P[String] = P(tqString | sqString)
}
