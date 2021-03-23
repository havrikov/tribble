package de.cispa.se.tribble
package generation

import scala.util.Random

private[tribble] class GoalBasedTreeGenerator(closeOffGenerator: TreeGenerator, random: Random)(implicit grammar: GrammarRepr, goal: CoverageGoal) extends ForestGenerator {

  private def gen(rule: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit goal: CoverageGoal): DTree = {
    goal.usedDerivation(rule, parent)
    if (goal.targetReached) {
      delegateToCloseOff(rule, parent, currentDepth)
    } else {
      rule match {
        case ref@Reference(name, _) =>
          val node = DNode(ref, parent)
          node.children(0) = gen(grammar(name), Some(node), currentDepth + 1)
          node
        case a@Alternation(alternatives, _) =>
          // take the alternative leading to the goal fastest
          val shortestAlts = minimalElementsBy(alternatives, goal.cost)
          val alternative = shortestAlts(random.nextInt(shortestAlts.length))
          val node = DNode(a, parent)
          node.children(0) = gen(alternative, Some(node), currentDepth + 1)
          node
        case c@Concatenation(elements, _) =>
          // problem with left recursion
          // we have to expand the closest-to-target element first!
          val node = DNode(c, parent)
          val toExpand = elements.zipWithIndex.sortBy { case (e, _) => goal.cost(e) }
          node.children ++= toExpand.map { case (e, i) => i -> gen(e, Some(node), currentDepth + 1) }
          node
        case q@Quantification(subj, min, _, _) =>
          val node = DNode(q, parent)
          node.children ++= Stream.fill(Math.max(min, 1))(subj).map(gen(_, Some(node), currentDepth + 1)).zipWithIndex.map(_.swap)
          node
        case t: TerminalRule =>
          // we do not use delegateToCloseOff here because we have already called goal.usedDerivation
          closeOffGenerator.gen(t, parent, currentDepth)
      }
    }
  }

  private def delegateToCloseOff(rule: DerivationRule, parent: Option[DNode], currentDepth: Int): DTree = {
    val node = closeOffGenerator.gen(rule, parent, currentDepth)
    // because we do not return to this method recursively when closing off the tree, we have to update the goal post-factum
    // the node itself, however, has already been reported to the goal at the beginning of the method gen
    node match {
      case DNode(_, _, children) => children.values.foreach(informGoal)
      case _ =>
    }
    node
  }

  @inline private def informGoal(t: DTree): Unit = t dfs { n => goal.usedDerivation(n.decl, n.parent) }


  /** Generates a forest satisfying the coverage goal */
  override def generateForest(): Stream[DTree] = {
    gen(grammar.root, None, 0) #:: (if (goal.nextTarget()) generateForest() else Stream.empty)
  }

}
