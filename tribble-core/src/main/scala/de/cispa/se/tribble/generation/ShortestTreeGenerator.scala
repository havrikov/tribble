package de.cispa.se.tribble
package generation

import scala.collection.mutable
import scala.util.Random

/** This tree generator generates the shortest possible derivation trees */
private[tribble] class ShortestTreeGenerator(regexGenerator: RegexGenerator, random: Random, closeOffChoice: Int) extends TreeGenerator {
  require(closeOffChoice > 0, s"Close-off choice hint must be positive! ($closeOffChoice given)")

  private[tribble] def gen(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = generate(rule, parent)(grammar, mutable.Map.empty.withDefaultValue(closeOffChoice - 1))

  private[tribble] def generate(rule: DerivationRule, parent: Option[DNode])(implicit grammar: GrammarRepr, closeOffs: mutable.Map[DerivationRule, Int]): DTree = rule match {
    case ref@Reference(name, _) =>
      val node = DNode(ref, parent)
      node.children(0) = generate(grammar.get(name), Some(node))
      node
    case a@Alternation(alternatives, _) =>
      // partition the alternatives by the shortest derivation and sort
      val partitions = alternatives.groupBy(_.shortestDerivation).toList.sortBy(_._1).map(_._2)
      // translate the close-off request into the partition number while clamping to the number of partitions
      var c = closeOffs(a)
      if (c >= partitions.size)
        c = partitions.size - 1
      // uniformly select an alternative from the requested partition
      val choices = partitions(c)
      val n = random.nextInt(choices.size)
      val alternative = choices.iterator.drop(n).next()
      val node = DNode(a, parent)
      // reduce the close-off requested for this node to ensure termination if we come by it again
      if (closeOffs(a) > 0)
        closeOffs(a) -= 1
      node.children(0) = generate(alternative, Some(node))
      node
    case c@Concatenation(elements, _) =>
      val node = DNode(c, parent)
      val trees = elements.map(generate(_, Some(node)))
      node.children ++= trees.indices zip trees
      node
    case q@Quantification(subj, min, _, _) =>
      val node = DNode(q, parent)
      val num = if (min == 0 && closeOffs(q) > 0) 1 else min
      if (closeOffs(q) > 0)
        closeOffs(q) -= 1
      node.children ++= Stream.fill(num)(subj).map(generate(_, Some(node))).zipWithIndex.map(_.swap)
      node
    case r: Regex =>
      DLeaf(r, parent, regexGenerator.generateIntoBuilder(r.automaton, new mutable.StringBuilder()).mkString)
    case l@Literal(value, _) =>
      DLeaf(l, parent, value)
  }

}
