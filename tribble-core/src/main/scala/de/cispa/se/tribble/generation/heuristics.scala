package de.cispa.se.tribble
package generation

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardOpenOption.APPEND

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Provides a way to choose the slot to expand next when generating derivation trees.
  */
trait Heuristic {
  /** Removes the next slot to fill from the given queue. */
  final def pickNext(q: mutable.ListBuffer[Slot]): Slot = {
    require(q.nonEmpty, "Cannot pick from an empty queue!")
    if (q.size == 1)
      q.remove(0)
    else
      makeChoice(q)
  }

  protected def makeChoice(q: mutable.ListBuffer[Slot]): Slot

  def createdNode(node: DTree): Unit = ()

  def startedTree(): Unit = ()

  def finishedTree(root: DTree): Unit = ()
}

/* *********************************************************** RANDOM CHOICE ***************************************************************** */

/** Provides a uniformly random choice. */
sealed class RandomChoice(random: Random) extends Heuristic {
  override def makeChoice(q: ListBuffer[Slot]): Slot = q.remove(random.nextInt(q.size))
}

/** Provides a uniformly random choice and also logs the number of uses for each reference  */
final class BookKeepingRandomChoice(random: Random, private val grammar: GrammarRepr) extends RandomChoice(random) {
  private val usages = mutable.Map[NonTerminal, Int]() withDefaultValue 0
  private val dumpFile = new File("dump_brandom.csv").toPath
  Files.write(dumpFile, grammar.rules.keySet.toSeq.sorted.mkString("size,", ",", "\n").getBytes(StandardCharsets.UTF_8))

  override def createdNode(node: DTree): Unit = node.decl match {
    case Reference(name, _) => usages(name) += 1
    case _ => // this particular choice is irrelevant to this heuristic
  }

  override def finishedTree(tree: DTree): Unit = {
    val usg = grammar.rules.keySet.toSeq.sorted.map(usages)
    Files.write(dumpFile, (tree.size() +: usg).mkString("", ",", "\n").getBytes(StandardCharsets.UTF_8), APPEND)
  }
}

final class LRUChoice(random: Random, private val resetForEachTree: Boolean = false) extends Heuristic {
  private val usages = mutable.Map[DerivationRule, Int]() withDefaultValue 0

  override protected def makeChoice(q: ListBuffer[Slot]): Slot = {
    val indexedUsages = q.map(_.decl).map(usages).zipWithIndex
    val minCount = indexedUsages.min._1
    val minimalCountElements = indexedUsages.filter(_._1 == minCount)
    val index = minimalCountElements(random.nextInt(minimalCountElements.size))._2
    q.remove(index)
  }

  override def createdNode(node: DTree): Unit = usages(node.decl) += 1

  override def startedTree(): Unit = if (resetForEachTree) usages.clear()
}

/* ******************************************************** NONTERMINAL METRICS ************************************************************** */


sealed abstract class AbstractUsageCoverage(random: Random, val grammar: GrammarRepr) extends NonTerminalReachability(grammar) with Heuristic  {

  private def select(q: ListBuffer[Slot]): Int = {
    case class Selection(score: Double, index: Int)
    val leastConsideredSelections = mutable.ListBuffer[Selection]()

    for ((slot@Slot(decl, _, _), i) <- q.zipWithIndex) {
      val reach = reachability(decl)

      val score: Double = if (reach.nonEmpty) reach.map { case (n, s) => computeScore(n.asInstanceOf[Reference].name, s, slot) }.min else Double.MaxValue

      if (leastConsideredSelections.isEmpty || score <= leastConsideredSelections.head.score) {
        leastConsideredSelections.prepend(Selection(score, i))
      }
    }
    assert(leastConsideredSelections.nonEmpty)

    // uniform selection among least elements
    val minScore = leastConsideredSelections.head.score // absolute minimum
    val candidates = leastConsideredSelections.takeWhile(_.score == minScore)
    val candidate = candidates(random.nextInt(candidates.size))

    candidate.index
  }

  override protected def makeChoice(q: ListBuffer[Slot]): Slot = {
    val index = select(q)
    q.remove(index)
  }

  protected def computeScore(nonTerm: NonTerminal, steps: Int, slot: Slot): Double
}

/** Prefer productions leading to yet unused ones (in a least-recently-used fashion) */
final class NonTerminalCoverage(random: Random, grammar: GrammarRepr) extends AbstractUsageCoverage(random, grammar) {
  /** tracks how often each nonterminal was used */
  private val usages = mutable.Map[NonTerminal, Int]() withDefaultValue 0
  /** the maximum possible distance to a nonterminal in the grammar */
  private val maxDistance = reachability(grammar.root).maxBy(_._2)._2

  override protected def computeScore(nonTerm: NonTerminal, steps: Int, slot: Slot): Double = maxDistance * usages(nonTerm) + steps

  override def createdNode(node: DTree): Unit = node.decl match {
    case Reference(name, _) => usages(name) += 1
    case _ => // this particular choice is irrelevant to this heuristic
  }

}

final class NWindowPairNonTerminalCoverage(n: Int, random: Random, grammar: GrammarRepr) extends AbstractUsageCoverage(random, grammar) {
  require(n > 0, s"n must be positive! ($n given)")
  /** tracks how often each pair of nonterminals was used one after another in a derivation tree */
  private val pairUsages = mutable.Map[(NonTerminal, NonTerminal), Int]() withDefaultValue 0
  // TODO possibly find a factor like maxDistance

  /** Computes the set of all pairs of nonterminals created by deriving `to` from `parent` */
  private def pairs(to: NonTerminal, parent: Option[DNode]): Set[(NonTerminal, NonTerminal)] = {
    val res = mutable.HashSet[(NonTerminal, NonTerminal)]()
    var p = parent
    var i = 0
    while (i < n && p.isDefined) {
      val node = p.get
      node.decl match {
        case Reference(name, _) => res.add(name -> to)
        case _ =>
      }
      i += 1
      p = node.parent
    }
    res.toSet
  }

  override protected def computeScore(nonTerm: NonTerminal, steps: Int, slot: Slot): Double = {
    // compute the set of all pairs created by choosing this nonterminal by calling pairs(nonTerm)
    val keys = pairs(nonTerm, Some(slot.parent))
    // choose the minimum value
    val value = if (keys.nonEmpty) keys.map(pairUsages).min else 0
    // apply scoring function
    10 * value + steps
  }

  override def createdNode(node: DTree): Unit = node.decl match {
    case Reference(name, _) => pairs(name, node.parent).foreach(pairUsages(_) += 1)
    case _ => // uninteresting
  }
}

final class KPathNonTerminalCoverage(k: Int, random: Random, grammar: GrammarRepr) extends AbstractUsageCoverage(random, grammar) {
  require(k > 0, s"k must be positive! ($k given)")
  private val usages = mutable.Map[List[NonTerminal], Int]() withDefaultValue 0

  private def getKTuple(last: NonTerminal, parent: Option[DNode]): List[NonTerminal] = {
    val res = mutable.ListBuffer[NonTerminal](last)
    var p = parent
    var i = 0
    while (i <= k && p.isDefined) {
      val node = p.get
      node.decl match {
        case Reference(name, _) =>
          res.prepend(name)
          i += 1
        case _ =>
        // we do not count this towards i
      }
      p = node.parent
    }
    res.toList
  }

  override protected def computeScore(nonTerm: NonTerminal, steps: Int, slot: Slot): Double = {
    // compute up-to-k-tuple ending in nonTerm
    val key = getKTuple(nonTerm, Some(slot.parent))
    // apply scoring function
    k * usages(key) + steps
  }

  override def createdNode(node: DTree): Unit = node.decl match {
    case Reference(name, _) => usages(getKTuple(name, node.parent)) += 1
    case _ => // uninteresting
  }
}


/* ************************************************** TERMINAL + NONTERMINAL METRICS ********************************************************* */

private[tribble] class KPathCoverage(k: Int, random: Random, val grammar: GrammarRepr, private val reach: Reachability) extends Heuristic {
  require(k > 0, s"k must be positive! ($k given)")
  private val usages = mutable.Map[List[DerivationRule], Int]() withDefaultValue 0

  private def computeScore(decl: DerivationRule, steps: Int, slot: Slot): Double = {
    // compute up-to-k-tuple ending in nonTerm
    val key = getKTuple(decl, Some(slot.parent))
    // apply scoring function
    k * usages(key) + steps
  }

  private def getKTuple(last: DerivationRule, parent: Option[DNode]): List[DerivationRule] = {
    val res = mutable.ListBuffer[DerivationRule](last)
    var p = parent
    var i = 0
    while (i <= k && p.isDefined) {
      val node = p.get
      node.decl match {
        case ref: Reference =>
          res.prepend(ref)
          i += 1
        case _ =>
        // we do not count this towards i
      }
      p = node.parent
    }
    res.toList
  }

  override def createdNode(node: DTree): Unit = node.decl match {
    case ref: Reference =>
      usages(getKTuple(ref, node.parent)) += 1
    case t: TerminalRule =>
      usages(getKTuple(t, node.parent)) += 1
    case _ => // uninteresting
  }

  override protected def makeChoice(q: ListBuffer[Slot]): Slot = {
    case class Selection(score: Double, index: Int)
    val leastConsideredSelections = mutable.ListBuffer[Selection]()

    for ((slot@Slot(decl, _, _), i) <- q.zipWithIndex) {
      // the following requires all paths from decl and their associated weights
      // we only count references as steps towards k but we do count all rules towards the score
      val declReach = reach.reachability(decl)
      val score: Double = if (declReach.nonEmpty) declReach.map { case (n, s) => computeScore(n, s, slot) }.min else Double.MaxValue
      if (leastConsideredSelections.isEmpty || score <= leastConsideredSelections.head.score) {
        leastConsideredSelections.prepend(Selection(score, i))
      }
    }
    assert(leastConsideredSelections.nonEmpty)

    // uniform selection among least elements
    val minScore = leastConsideredSelections.head.score // absolute minimum
    val candidates = leastConsideredSelections.takeWhile(_.score == minScore)
    val candidate = candidates(random.nextInt(candidates.size))

    q.remove(candidate.index)
  }
}
