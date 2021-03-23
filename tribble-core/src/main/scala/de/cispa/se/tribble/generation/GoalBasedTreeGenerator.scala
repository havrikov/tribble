package de.cispa.se.tribble
package generation

import scala.util.Random

private[tribble] class GoalBasedTreeGenerator(closeOffGenerator: TreeGenerator, goal: CoverageGoal, random: Random)(implicit val grammar: GrammarRepr) extends ForestGenerator {

  private def gen(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit goal: CoverageGoal): DTree = {
    goal.usedDerivation(rule, parent)
    rule match {
      case ref@Reference(name, _) =>
        val node = DNode(ref, parent)
        node.children(0) = gen(grammar(name), Some(node), currentDepth + 1)
        node
      case a@Alternation(alternatives, _) =>
        // if the current target is not yet reached, take an alternative leading there fastest
        if (!goal.targetReached) {
          val shortest_alts = minimalElementsBy(alternatives, goal.cost)
          val alternative = shortest_alts(random.nextInt(shortest_alts.length))
          val node = DNode(a, parent)
          node.children(0) = gen(alternative, Some(node), currentDepth + 1)
          node
        } else {
          // if we already reached the target, close off the tree
          delegateToCloseOff(a, parent, currentDepth)
        }
      case c@Concatenation(elements, _) =>
        // problem with left recursion
        // we have to expand the closest-to-target element first!
        val node = DNode(c, parent)
        val toExpand = if (goal.targetReached) {
          elements.zipWithIndex
        } else {
          elements.zipWithIndex.sortBy { case (e, _) => goal.cost(e) }
        }
        val generated_children = toExpand.map { case (e, i) => i -> gen(e, Some(node), currentDepth + 1) }
        node.children ++= generated_children
        node
      case q@Quantification(subj, min, _, _) =>
        if (goal.targetReached) {
          delegateToCloseOff(q, parent, currentDepth)
        } else {
          val node = DNode(q, parent)
          node.children ++= Stream.fill(Math.max(min, 1))(subj).map(gen(_, Some(node), currentDepth + 1)).zipWithIndex.map(_.swap)
          node
        }
      case t: TerminalRule => closeOffGenerator.gen(t, parent, currentDepth)
    }
  }

  private def delegateToCloseOff(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit goal: CoverageGoal): DTree = {
    val node = closeOffGenerator.gen(rule, parent, currentDepth)
    // because we do not return to this method recursively when closing off the tree, we have to update the goal post-factum
    // the node itself, however, has already been reported to the goal at the beginning of the method gen
    node match {
      case DNode(_, _, children) => children.values.foreach(informGoal)
      case _ =>
    }
    node
  }

  private def informGoal(t: DTree)(implicit goal: CoverageGoal): Unit = t dfs { n => goal.usedDerivation(n.decl, n.parent) }

  private def generate(grammar: GrammarRepr, goal: CoverageGoal): Stream[DTree] = gen(grammar.root, None, 0)(goal) #:: (if (goal.nextTarget()) generate(grammar, goal) else Stream.empty)

  /** Generates a forest satisfying the coverage goal */
  override def generateForest(): Stream[DTree] = generate(grammar, goal)

}
