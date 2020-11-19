package de.cispa.se.tribble
package output

class GrammarPrettyPrinter(private val grammar: GrammarRepr) {

  def prettyPrint(printID: Boolean = false, printProb: Boolean = false): String = {
    val builder = new StringBuilder("Grammar(\n")
    for ((terminal, rule) <- grammar.rules) {
      builder.append(s"'$terminal := ")
      builder.append(print(rule, printID, printProb))
      builder.append(",\n")
    }
    builder.replace(builder.length - 2, builder.length, "\n)\n")
    builder.mkString
  }

  private def print(rule: DerivationRule, printID: Boolean, printProb: Boolean): String = {
    var res = rule match {
      case Reference(name, id) =>
        var res = s"'$name"
        if (printID) {
          res += appendId(id)
        }
        res
      case Concatenation(elements, id) => recurseElements(elements, " ~ ", printID, printProb) + appendId(id)
      case a: Alternation => recurseElements(a.alternatives, " | ", printID, printProb) + appendId(a.id)
      case Quantification(subject, min, max, id) => s"${print(subject, printID, printProb)}${
        (min, max) match {
          case (0, 1) => ".?"
          case (0, Int.MaxValue) => ".rep"
          case (1, Int.MaxValue) => ".rep(1)"
          case _ => s".rep($min,$max)"
        }
      }" + appendId(id)
      case Literal(value, id) =>
        var res = fastparse.internal.Util.literalize(value, unicode = true)
        if (printID) {
          res += appendId(id)
        }
        res
      case Regex(value, id) =>
        var res = fastparse.internal.Util.literalize(value, unicode = true) + ".regex"
        if (printID) {
          res += appendId(id)
        }
        res
    }
    if (printProb) {
      res += probability(rule)
    }
    res
  }

  private def recurseElements(elements: Seq[DerivationRule], separator: String, printID: Boolean, printProb: Boolean) = {
    val builder = new StringBuilder("(")
    for (elem <- elements) {
      builder.append(print(elem, printID, printProb))
      builder.append(separator)
    }
    builder.replace(builder.length - separator.length, builder.length, ")")
    builder.mkString
  }

  private def appendId(id: Int): String = if (id != 0) s"/*@$id*/" else ""

  private def probability(rule: DerivationRule): String = if (rule.probability.isNaN) "" else s" @@ ${rule.probability}"
}
