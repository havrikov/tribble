package de.cispa.se.tribble
package input

/** Resets the id of all derivation rules to [[DerivationRule.DEFAULT_ID]]. Useful only in testing. */
private[input] object ResetIds extends AssemblyPhase {
  override def process(grammar: GrammarRepr): GrammarRepr = {
    grammar.rules.values.flatMap(_.toStream).view.force.foreach(_.id = DerivationRule.DEFAULT_ID)
    grammar
  }
}
