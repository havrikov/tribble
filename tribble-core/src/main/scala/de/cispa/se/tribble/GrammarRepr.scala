package de.cispa.se.tribble

private[tribble] final case class GrammarRepr(start: NonTerminal, rules: Map[NonTerminal, DerivationRule]) {
  def root: DerivationRule = rules(start)
  def get(reference: Reference): DerivationRule = rules(reference.name)
  def get(nonTerminal: NonTerminal): DerivationRule = rules(nonTerminal)

  /** @return the grammar created by adding the given production */
  private[tribble] def +(production: Production): GrammarRepr = {
    val (lhs, _) = production
    if (rules.contains(lhs))
      throw new IllegalArgumentException(s"Cannot have multiple declarations for $lhs!")
    copy(rules = rules + production)
  }
}
