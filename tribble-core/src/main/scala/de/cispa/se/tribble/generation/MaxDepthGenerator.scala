package de.cispa.se.tribble
package generation

import org.log4s.getLogger

import scala.collection.mutable
import scala.util.Random

class MaxDepthGenerator(maxRepetitions: Int, random: Random, regexGenerator: RegexGenerator, maxDepth: Int, heuristic: Heuristic) extends TreeGenerator {
  require(maxDepth > 0, s"The maximum depth must be positive ($maxDepth given).")
  private val logger = getLogger

  override def gen(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    heuristic.startedTree()
    val tree = limitedGenerate(rule, parent, currentDepth)
    heuristic.finishedTree(tree)
    tree
  }

  private def limitedGenerate(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = rule match {
    case ref@Reference(name, _) =>
      val node = DNode(ref, parent)
      heuristic.createdNode(node)
      node.children(0) = limitedGenerate(grammar.get(name), Some(node), currentDepth + 1)
      node
    case r: Regex =>
      val leaf = DLeaf(r, parent, regexGenerator.generateIntoBuilder(r.automaton, new mutable.StringBuilder()).mkString)
      heuristic.createdNode(leaf)
      leaf
    case l@Literal(value, _) =>
      val leaf = DLeaf(l, parent, value)
      heuristic.createdNode(leaf)
      leaf
    case a@Alternation(alternatives, _) =>
      val node = DNode(a, parent)
      heuristic.createdNode(node)
      // filter alternatives by depth
      val alternative = selectAlternative(alternatives, currentDepth, node)
      node.children(0) = limitedGenerate(alternative, Some(node), currentDepth + 1)
      node
    case c@Concatenation(elements, _) =>
      val node = DNode(c, parent)
      heuristic.createdNode(node)
      val trees = elements.map(limitedGenerate(_, Some(node), currentDepth + 1))
      node.children ++= trees.indices zip trees
      node
    case q@Quantification(subj, min, max, _) =>
      val constrainedMax = Math.max(min, Math.min(max, maxRepetitions))
      val node = DNode(q, parent)
      heuristic.createdNode(node)
      if (min > 0 || currentDepth + subj.shortestDerivation < maxDepth) {
        val num = min + random.nextInt(constrainedMax - min + 1)
        node.children ++= Stream.fill(num)(subj).map(limitedGenerate(_, Some(node), currentDepth + 1)).zipWithIndex.map(_.swap)
      } else {
        logger.trace(s"Stopping derivation at depth $currentDepth")
      }
      node
  }

  protected def selectAlternative(alternatives: Seq[DerivationRule], currentDepth: Int, parent: DNode): DerivationRule = {
    val fittingAlts = alternatives.filter(_.shortestDerivation + currentDepth < maxDepth)
    require(fittingAlts.nonEmpty, s"Impossible to produce a derivation not deeper than $maxDepth! (I need at least ${parent.decl.shortestDerivation} steps from depth $currentDepth)")
    // let the heuristic select the best alternative
    val slots = mutable.ListBuffer(fittingAlts.map(Slot(_, 0, parent)): _*)
    val Slot(alternative, _, _) = heuristic.pickNext(slots)
    logger.trace(s"Choosing at depth $currentDepth the alternative $alternative because it needs ${alternative.shortestDerivation} steps")
    alternative
  }


}
