package de.cispa.se.tribble
package generation

import org.log4s.getLogger

import scala.collection.mutable
import scala.util.Random

private[tribble] class NaiveTreeGenerator(maxRepetitions: Int, regexGenerator: RegexGenerator, maxDepth: Int, random: Random) extends TreeGenerator {
  require(maxDepth > 0, s"The maximum depth must be positive ($maxDepth given).")
  private[this] val logger = getLogger

  override private[tribble] def gen(rule: DerivationRule, parent: Option[DNode], depth: Int)(implicit grammar: GrammarRepr): DTree = {
    rule match {
      case ref@Reference(name, _) =>
        val node = DNode(ref, parent)
        node.children(0) = gen(grammar.get(name), Some(node), depth + 1)
        node
      case r: Regex =>
        DLeaf(r, parent, regexGenerator.generateIntoBuilder(r.automaton, new mutable.StringBuilder()).mkString)
      case l@Literal(value, _) =>
        DLeaf(l, parent, value)
      case a@Alternation(alternatives, _) =>
        // uniform selection across alternation
        val n = random.nextInt(alternatives.size)
        val alternative = alternatives.iterator.drop(n).next
        val node = DNode(a, parent)
        node.children(0) = gen(alternative, Some(node), depth + 1)
        node
      case c@Concatenation(elements, _) =>
        val node = DNode(c, parent)
        val trees = elements.map(gen(_, Some(node), depth + 1))
        node.children ++= trees.indices zip trees
        node
      case q@Quantification(subj, min, max, _) =>
        val constrainedMax = Math.max(min, Math.min(max, maxRepetitions))
        val node = DNode(q, parent)
        if (min > 0 || depth < maxDepth) {
          val num = min + random.nextInt(constrainedMax - min + 1)
          node.children ++= Stream.fill(num)(subj).map(gen(_, Some(node), depth + 1)).zipWithIndex.map(_.swap)
        } else {
          logger.trace(s"Stopping derivation at depth $depth")
        }
        node
    }
  }
}
