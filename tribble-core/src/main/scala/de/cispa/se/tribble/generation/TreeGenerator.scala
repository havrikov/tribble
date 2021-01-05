package de.cispa.se.tribble
package generation

trait TreeGenerator {
  def generate(implicit grammar: GrammarRepr): DTree = gen(grammar.root, None, 0)

  private[tribble] def gen(decl: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree
}
