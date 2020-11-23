package de.cispa.se.tribble
package generation

import org.log4s.getLogger

import scala.collection.mutable
import scala.util.Random

class MaxDepthGenerator(maxRepetitions: Int, random: Random, regexGenerator: RegexGenerator, maxDepth: Int, heuristic: Heuristic) extends NaiveTreeGenerator(maxRepetitions, regexGenerator, maxDepth, random) {
  private val logger = getLogger

  override protected def startGenerate(): Unit = heuristic.startedTree()

  override protected def finishGenerate(tree: DTree): Unit = heuristic.finishedTree(tree)

  override protected def prepareNode(tree: DTree): Unit = heuristic.createdNode(tree)

  override protected def canExpandQuantification(q: Quantification, currentDepth: Int): Boolean =
    currentDepth + q.subject.shortestDerivation < maxDepth

  override protected def instantiateAlternation(alternation: Alternation, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val node = DNode(alternation, parent)
    prepareNode(node)
    val alternative = {
      // filter alternatives by depth
      val fittingAlts = alternation.alternatives.filter(_.shortestDerivation + currentDepth < maxDepth)
      if (fittingAlts.nonEmpty) {
        // let the heuristic select the best alternative
        val slots = mutable.ListBuffer(fittingAlts.map(Slot(_, 0, node)): _*)
        val Slot(alternative, _, _) = heuristic.pickNext(slots)
        logger.trace(s"Choosing at depth $currentDepth the alternative $alternative because it needs ${alternative.shortestDerivation} steps")
        alternative
      } else {
        handleNoFittingAlts(alternation.alternatives, currentDepth, node)
          .getOrElse(throw new IllegalArgumentException(s"Impossible to produce a derivation not deeper than $maxDepth! (I need at least ${alternation.shortestDerivation} steps from depth $currentDepth)"))
      }
    }
    node.children(0) = gen(alternative, Some(node), currentDepth + 1)
    node
  }

  protected def handleNoFittingAlts(alternatives: Seq[DerivationRule], currentDepth: Int, parent: DNode): Option[DerivationRule] = None
}
