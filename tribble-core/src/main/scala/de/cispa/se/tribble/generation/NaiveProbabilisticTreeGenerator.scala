package de.cispa.se.tribble
package generation

import scala.util.Random

class NaiveProbabilisticTreeGenerator(maxRepetitions: Int, regexGenerator: RegexGenerator, maxDepth: Int, random: Random, shortestTreeGenerator: ShortestTreeGenerator, cutoffDepth: Int = Int.MaxValue) extends NaiveTreeGenerator(maxRepetitions, regexGenerator, maxDepth, random) {

  override protected def alternativeGenerationMethod(decl: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): Option[DTree] = {
    if (currentDepth <= cutoffDepth) {
      None
    } else {
      Some(shortestTreeGenerator.gen(decl, parent, currentDepth))
    }
  }

  override protected def instantiateAlternation(alternation: Alternation, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    // break out from set to have one order
    val orderedAlts = alternation.alternatives
    var p = random.nextDouble()

    var n = 0
    while (n < orderedAlts.size && p > 0) {
      p -= orderedAlts(n).probability
      n += 1
    }
    val alternative = orderedAlts(n - 1)

    val node = DNode(alternation, parent)
    node.children(0) = gen(alternative, Some(node), currentDepth + 1)
    node
  }
}
