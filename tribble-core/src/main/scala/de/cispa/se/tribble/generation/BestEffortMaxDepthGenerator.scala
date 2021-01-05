package de.cispa.se.tribble
package generation

import scala.collection.mutable
import scala.util.Random

class BestEffortMaxDepthGenerator(maxRepetitions: Int, random: Random, regexGenerator: RegexGenerator, maxDepth: Int, heuristic: Heuristic) extends MaxDepthGenerator(maxRepetitions, random, regexGenerator, maxDepth, heuristic) {
  override protected def selectAlternative(alternatives: Seq[DerivationRule], currentDepth: Int, parent: DNode): DerivationRule = {
    val fittingAlts = alternatives.filter(_.shortestDerivation + currentDepth <= maxDepth)
    if (fittingAlts.isEmpty) {
      alternatives.minBy(_.shortestDerivation)
    } else {
      // let the heuristic select the best alternative
      val slots = mutable.ListBuffer(fittingAlts.map(Slot(_, 0, parent)): _*)
      val Slot(alternative, _, _) = heuristic.pickNext(slots)
      alternative
    }
  }
}
