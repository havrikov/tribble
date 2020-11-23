package de.cispa.se.tribble
package generation

import org.log4s.getLogger

import scala.util.Random

class NaiveTreeGenerator(maxRepetitions: Int, regexGenerator: RegexGenerator, maxDepth: Int, random: Random) extends RecursiveTreeGenerator(regexGenerator) {
  require(maxDepth > 0, s"The maximum depth must be positive ($maxDepth given).")
  private val logger = getLogger

  override protected def instantiateAlternation(alternation: Alternation, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    // uniform selection across alternation
    val alternatives = alternation.alternatives
    val n = random.nextInt(alternatives.size)
    val alternative = alternatives.iterator.drop(n).next
    val node = DNode(alternation, parent)
    prepareNode(node)
    node.children(0) = gen(alternative, Some(node), currentDepth + 1)
    node
  }

  override protected def instantiateQuantification(quantification: Quantification, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val q@Quantification(subj, min, max, _) = quantification
    val constrainedMax = Math.max(min, Math.min(max, maxRepetitions))
    val node = DNode(q, parent)
    prepareNode(node)
    if (min > 0 || canExpandQuantification(q, currentDepth)) {
      val num = min + random.nextInt(constrainedMax - min + 1)
      node.children ++= Stream.fill(num)(subj).map(gen(_, Some(node), currentDepth + 1)).zipWithIndex.map(_.swap)
    } else {
      logger.trace(s"Stopping derivation at depth $currentDepth")
    }
    node
  }

  protected def canExpandQuantification(q: Quantification, currentDepth: Int): Boolean = currentDepth < maxDepth
}
