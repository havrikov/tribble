package de.cispa.se.tribble

final case class GrammarRepr(start: NonTerminal, rules: Map[NonTerminal, DerivationRule]) {
  def root: DerivationRule = rules(start)
  def apply(reference: Reference): DerivationRule = get(reference.name)
  def apply(nonTerminal: NonTerminal): DerivationRule = get(nonTerminal)
  def get(reference: Reference): DerivationRule = get(reference.name)
  def get(nonTerminal: NonTerminal): DerivationRule = rules.getOrElse(nonTerminal,
    throw new NoSuchElementException(s"The grammar has no rule named $nonTerminal"))
}
