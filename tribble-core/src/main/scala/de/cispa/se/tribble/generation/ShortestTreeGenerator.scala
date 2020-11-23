package de.cispa.se.tribble
package generation

import scala.collection.mutable
import scala.util.Random

/** This tree generator generates the shortest possible derivation trees */
class ShortestTreeGenerator(regexGenerator: RegexGenerator, random: Random, closeOffChoice: Int) extends RecursiveTreeGenerator(regexGenerator) {
  require(closeOffChoice > 0, s"Close-off choice hint must be positive! ($closeOffChoice given)")
  private val closeOffs: mutable.Map[DerivationRule, Int] = mutable.Map.empty.withDefaultValue(closeOffChoice - 1)

  override protected def startGenerate(): Unit = {
    closeOffs.clear()
  }

  override protected def instantiateAlternation(alternation: Alternation, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val alternatives = alternation.alternatives
    // partition the alternatives by the shortest derivation and sort
    val partitions = alternatives.groupBy(_.shortestDerivation).toList.sortBy(_._1).map(_._2)
    // translate the close-off request into the partition number while clamping to the number of partitions
    var c = closeOffs(alternation)
    if (c >= partitions.size)
      c = partitions.size - 1
    // uniformly select an alternative from the requested partition
    val choices = partitions(c)
    val n = random.nextInt(choices.size)
    val alternative = choices.iterator.drop(n).next()
    val node = DNode(alternation, parent)
    prepareNode(node)
    // reduce the close-off requested for this node to ensure termination if we come by it again
    if (closeOffs(alternation) > 0)
      closeOffs(alternation) -= 1
    node.children(0) = gen(alternative, Some(node), currentDepth + 1)
    node
  }

  override protected def instantiateQuantification(quantification: Quantification, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val q@Quantification(subj, min, _, _) = quantification
    val node = DNode(q, parent)
    prepareNode(node)
    val num = if (min == 0 && closeOffs(q) > 0) 1 else min
    if (closeOffs(q) > 0)
      closeOffs(q) -= 1
    node.children ++= Stream.fill(num)(subj).map(gen(_, Some(node), currentDepth + 1)).zipWithIndex.map(_.swap)
    node
  }
}
