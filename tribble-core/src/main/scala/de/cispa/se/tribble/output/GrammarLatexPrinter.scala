package de.cispa.se.tribble
package output

class GrammarLatexPrinter(private val grammar: GrammarRepr) {

  def prettyPrint(): String = {
    val builder = new StringBuilder()
    for ((terminal, rule) <- grammar.rules) {
      builder.append(s"\\nonterm{$terminal} \\expandsto ")
      builder.append(print(rule))
      builder.append(";\n")
    }
    builder.mkString
  }

  private def print(rule: DerivationRule): String = rule match {
    case Reference(name, _) => s"\\nonterm{$name}"
    case Concatenation(elements) => recurseElements(elements, " ")
    case a: Alternation => recurseElements(a.alternatives, " | ")
    case Quantification(subject, min, max) => s"${print(subject)}${
      (min, max) match {
        case (0, 1) => ".?"
        case (0, Int.MaxValue) => ".rep"
        case (1, Int.MaxValue) => ".rep(1)"
        case _ => s".rep($min,$max)"
      }
    }"
    case Literal(value, _) =>
      val res = fastparse.internal.Util.literalize(value, unicode = true)
      s"\\term{$res}"
    case Regex(value, _) =>
      val res = fastparse.internal.Util.literalize(value, unicode = true)
      s"\\regex{$res}"
  }

  private def recurseElements(elements: Seq[DerivationRule], separator: String) = {
    val builder = new StringBuilder("(")
    for (elem <- elements) {
      builder.append(print(elem))
      builder.append(separator)
    }
    builder.replace(builder.length - separator.length, builder.length, ")")
    builder.mkString
  }

}
