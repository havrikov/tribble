package saarland.cispa.se.tribble
package execution

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import saarland.cispa.se.tribble.generation.KPathCoverageGoal
import saarland.cispa.se.tribble.model.{DLeaf, DNode, DTree}

import scala.collection.immutable.TreeMap
import scala.util.Random

sealed trait TreeReporter {
  def processTree(index: Int, tree: DTree): Unit
}

object NopReporter extends TreeReporter {
  override def processTree(index: Int, tree: DTree): Unit = ()
}


class KPathReporter(coverageReport: File, maxK: Int)(implicit grammar: GrammarRepr, implicit val random: Random) extends TreeReporter {
  require(maxK > 0, s"MaxK must be positive! ($maxK given)")
  private val goals = TreeMap((1 to maxK).map(x => x -> new KPathCoverageGoal(x)): _*)
  private val targetCounts = goals.mapValues(_.targetCount.toDouble)
  private val report = CSVWriter.open(coverageReport, append = false, encoding = "UTF-8")
  report.writeRow("numfiles" :: goals.keysIterator.map(i => s"$i-path").toList)

  private def updateGoal(tree: DTree): Unit = tree match {
    case DNode(decl, parent, children) =>
      for (goal <- goals.valuesIterator) {
        goal.usedDerivation(decl, parent)
      }
      children.values.foreach(updateGoal)
    case DLeaf(decl, parent, _) =>
      for (goal <- goals.valuesIterator)
        goal.usedDerivation(decl, parent)
  }

  override def processTree(index: Int, tree: DTree): Unit = {
    updateGoal(tree)
    report.writeRow(index :: goals.map {case (k, goal)  => goal.coveredTargets / targetCounts(k)}.toList)
  }
}
