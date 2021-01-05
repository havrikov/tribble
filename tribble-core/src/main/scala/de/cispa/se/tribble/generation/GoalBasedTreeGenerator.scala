package de.cispa.se.tribble
package generation

import scala.util.Random

private[tribble] class GoalBasedTreeGenerator(closeOffGenerator: TreeGenerator, goal: CoverageGoal, random: Random)(implicit val grammar: GrammarRepr) extends ForestGenerator {

  private def gen(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit goal: CoverageGoal): DTree = {
    goal.usedDerivation(rule, parent)
    rule match {
      case ref@Reference(name, _) =>
        val node = DNode(ref, parent)
        node.children(0) = gen(grammar.get(name), Some(node), currentDepth + 1)
        node
      case a@Alternation(alternatives) =>
        // if the current target is not yet reached, take an alternative leading there fastest
        if (!goal.targetReached) {
          val shortest_alts = minimalElementsBy(alternatives, goal.cost)
          val alternative = shortest_alts(random.nextInt(shortest_alts.length))
          val node = DNode(a, parent)
          node.children(0) = gen(alternative, Some(node), currentDepth + 1)
          node
        } else {
          // if we already reached the target, close off the tree
          val node = closeOffGenerator.gen(a, parent, currentDepth + 1)
          // because we do not return to this method recursively when closing off the tree, we have to update the goal post-factum
          // the node itself, however, has already been reported to the goal at the beginning of this method
          node match {
            case DNode(_, _, children) => children.values.foreach(informGoal)
            case _ =>
          }
          node
        }
      case c@Concatenation(elements) =>
        // problem with left recursion
        // we have to expand the closest-to-target element first!
        val node = DNode(c, parent)
        val sorted_by_closeness = elements.zipWithIndex.sortBy { case (e, _) => goal.cost(e) }
        val generated_children = sorted_by_closeness.map { case (e, i) => i -> gen(e, Some(node), currentDepth + 1) }
        node.children ++= generated_children
        node
      case q@Quantification(subj, min, _) =>
        val node = DNode(q, parent)
        if (goal.targetReached) {
          val newChildren = Stream.fill(min)(subj).map(closeOffGenerator.gen(_, Some(node), currentDepth + 1)).toList
          // we do not recurse here, so update the goal post-factum
          newChildren.foreach(informGoal)
          node.children ++= newChildren.zipWithIndex.map(_.swap)
        } else {
          node.children ++= Stream.fill(Math.max(min, 1))(subj).map(gen(_, Some(node), currentDepth + 1)).zipWithIndex.map(_.swap)
        }
        node
      case t: TerminalRule => closeOffGenerator.gen(t, parent, currentDepth + 1)
    }
  }

  private def informGoal(t: DTree)(implicit goal: CoverageGoal): Unit = t dfs { n => goal.usedDerivation(n.decl, n.parent) }

  private def generate(grammar: GrammarRepr, goal: CoverageGoal): Stream[DTree] = gen(grammar.root, None, 0)(goal) #:: (if (goal.nextTarget()) generate(grammar, goal) else Stream.empty)

  /** Generates a forest satisfying the coverage goal */
  override def generateForest(): Stream[DTree] = generate(grammar, goal)

}
