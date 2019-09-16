package saarland.cispa.se.tribble.generation

import saarland.cispa.se.tribble.model.{DNode, DerivationRule}

import scala.collection.mutable
import scala.util.Random

class BestEffortMaxDepthGenerator(random: Random, regexGenerator: RegexGenerator, maxDepth: Int, heuristic: Heuristic) extends MaxDepthGenerator(random, regexGenerator, maxDepth, heuristic) {
  override protected def selectAlternative(alternatives: Set[DerivationRule], currentDepth: Int, parent: DNode): DerivationRule = {
    val fittingAlts = alternatives.filter(_.shortestDerivation + currentDepth <= maxDepth)
    if (fittingAlts.isEmpty) {
      alternatives.minBy(_.shortestDerivation)
    } else {
      // let the heuristic select the best alternative
      val slots = mutable.ListBuffer(fittingAlts.map(Slot(_, 0, parent)).toSeq: _*)
      val Slot(alternative, _, _) = heuristic.pickNext(slots)
      alternative
    }
  }
}
