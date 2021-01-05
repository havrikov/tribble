package de.cispa.se.tribble
package model

import scala.collection.mutable

/**
  * Keeps track of parents of all derivation rules in the grammar
  */
class ParentTracker(private val grammar: GrammarRepr) {

  private val parentMap: mutable.Map[DerivationRule, DerivationRule] = mutable.Map.empty

  for (rule <- grammar.rules.values) {
    parentMap(rule) = rule
    setParent(rule)
  }

  private def setParent(parent: DerivationRule): Unit = {
    for (child <- parent.children) {
      parentMap(child) = parent
      setParent(child)
    }
  }

  /**
    * Finds the parent of the given derivation rule
    *
    * @return Some(parent) if there is one, or None if the rule is the complete right hand side of a production
    * @throws NoSuchElementException if the decl was not found in the grammar
    */
  def parent(decl: DerivationRule): Option[DerivationRule] = {
    val p = parentMap.getOrElse(decl, throw new NoSuchElementException(s"$decl not found!"))
    if (p != decl) Some(p) else None
  }

  // used for testing
  private[model] def size: Int = parentMap.size
}
