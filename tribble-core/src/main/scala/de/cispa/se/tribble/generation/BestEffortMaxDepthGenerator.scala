package de.cispa.se.tribble
package generation

import scala.util.Random

class BestEffortMaxDepthGenerator(maxRepetitions: Int, random: Random, regexGenerator: RegexGenerator, maxDepth: Int, heuristic: Heuristic) extends MaxDepthGenerator(maxRepetitions, random, regexGenerator, maxDepth, heuristic) {

  override protected def handleNoFittingAlts(alternatives: Seq[DerivationRule], currentDepth: Int, parent: DNode): Option[DerivationRule] =
    Some(alternatives.minBy(_.shortestDerivation))
}
