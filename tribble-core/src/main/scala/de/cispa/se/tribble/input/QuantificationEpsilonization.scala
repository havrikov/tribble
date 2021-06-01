package de.cispa.se.tribble
package input

/**
  * Turns quantifications of the form `foo{0,x}` into alternations of the form `"" | foo{1,x}`.
  * <p>
  * This can be useful to make the k-path algorithm consider absent optionals as interesting.
  * <p>
  * <b>Run [[AssignIds]] afterwards to give newly created rules unique ids!</b>
  */
object QuantificationEpsilonization extends AssemblyPhase {
  override def process(grammar: GrammarRepr): GrammarRepr = grammar.copy(rules = grammar.rules.mapValues(epsilonize).view.force)

  private def epsilonize(rule: DerivationRule): DerivationRule = {
    val rewritten = rule match {
      case c: Concatenation => c.copy(c.elements.map(epsilonize))
      case a: Alternation => a.copy(a.alternatives.map(epsilonize))
      case Quantification(subject, 0, 1, _) => Alternation(Seq(Literal(""), epsilonize(subject)))
      case Quantification(subject, 0, max, id) => Alternation(Seq(Literal(""), Quantification(epsilonize(subject), 1, max, id)))
      case q: Quantification => q.copy(epsilonize(q.subject))
      case _ => rule
    }

    rewritten.probability = rule.probability

    // Setting the shortestDerivation on a terminal throws an IllegalArgumentException :(
    if (!rule.isInstanceOf[TerminalRule]) {
      rewritten.shortestDerivation = rule.shortestDerivation
    }

    rewritten
  }
}
