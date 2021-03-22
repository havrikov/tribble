package de.cispa.se.tribble
package generation

import org.log4s.getLogger

import scala.collection.mutable
import scala.util.Random

trait ForestGenerator {
  def generateForest(): Iterable[DTree]
}

class ForestAdapter(treeGenerator: TreeGenerator, n: Int)(implicit grammar: GrammarRepr) extends ForestGenerator {
  override def generateForest(): Stream[DTree] = Stream.continually(treeGenerator.generate).take(n)
}

class ForestSizeLimiter(generator: ForestGenerator, n: Int) extends ForestGenerator {
  override def generateForest(): Iterable[DTree] = generator.generateForest().take(n)
}

class ForestTimeLimiter(generator: ForestGenerator, minutes: Int) extends ForestGenerator {
  override def generateForest(): Iterable[DTree] = {
    val timeout = minutes * 60 * 1000
    val start = System.currentTimeMillis()
    generator.generateForest() takeWhile { _ => System.currentTimeMillis() < start + timeout }
  }
}

class ContinuingForestAdapter(generator: ForestGenerator, treeGenerator: TreeGenerator, n: Int)(implicit grammar: GrammarRepr) extends ForestGenerator {
  override def generateForest(): Iterable[DTree] = (generator.generateForest().iterator ++ Iterator.continually(treeGenerator.generate)).toStream.take(n)
}

class SizedForestAdapter(min: Int, max: Int, n: Int, heuristic: Heuristic, maxRepetitions: Int)(implicit grammar: GrammarRepr, random: Random, shortestTreeGenerator: ShortestTreeGenerator) extends ForestGenerator {
  require(min < max, s"Min must be smaller than Max! ($min >= $max given)")
  private val logger = getLogger

  override def generateForest(): Iterable[DTree] = {
    var size = (max - min) / 2
    val result = mutable.ListBuffer[DTree]()

    while (result.size < n) {
      val generator = new SizedTreeGenerator(maxRepetitions, random, shortestTreeGenerator, size, heuristic)
      val tree = generator.generate
      val treeSize = tree.size()
      if (treeSize < min) {
        size += 1
        logger.debug(s"Correcting desired tree size up to $size")
      } else if (treeSize > max) {
        size -= 1
        logger.debug(s"Correcting desired tree size down to $size")
      } else
        result += tree
    }
    result
  }
}
