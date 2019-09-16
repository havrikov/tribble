package saarland.cispa.se.tribble
package generation

import saarland.cispa.se.tribble.model._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Provides a way to choose the slot to expand next when generating derivation trees.
  */
sealed trait Heuristic {
  /** removes the next slot to fill from the given queue */
  final def pickNext(q: mutable.ListBuffer[Slot]): Slot = {
    require(q.nonEmpty, "Cannot pick from an empty queue!")
    if (q.lengthCompare(1) == 0)
      q.remove(0)
    else
      makeChoice(q)
  }

  protected def makeChoice(q: mutable.ListBuffer[Slot]): Slot

  def createdNode(node: DTree): Unit

  def startedTree(): Unit

  def finishedTree(root: DTree): Unit
}

/* *********************************************************** RANDOM CHOICE ***************************************************************** */

/**
  * Provides a uniformly random choice
  */
final class RandomChoice(random: Random) extends Heuristic {
  override def makeChoice(q: ListBuffer[Slot]): Slot = q.remove(random.nextInt(q.size))

  override def createdNode(node: DTree): Unit = ()

  override def startedTree(): Unit = ()

  override def finishedTree(root: DTree): Unit = ()
}

trait ReachabilityInformation {
  def grammar: GrammarRepr
  protected def computeImmediateSteps(decl: DerivationRule, steps: Int = 0): Map[DerivationRule, Int] = decl match {
    case ref:Reference => Map(ref -> steps)
    case Concatenation(elements) => elements.flatMap(computeImmediateSteps(_, steps + 1))(collection.breakOut)
    case Alternation(alternatives) => alternatives.flatMap(computeImmediateSteps(_, steps + 1))(collection.breakOut)
    case Quantification(subject, _, _) => computeImmediateSteps(subject, steps + 1)
    case t: TerminalRule => Map(t -> steps)
  }

  private def reachable(decl: DerivationRule, transitions: mutable.Map[NonTerminal, Map[DerivationRule, Int]]): mutable.Map[DerivationRule, Int] = {
    val current = mutable.HashMap[DerivationRule, Int]()
    val seen = mutable.HashSet[NonTerminal]()
    // initialize with immediately reachable nonterminals and terminals
    decl match {
      case ref@Reference(name,_) =>
        current ++= transitions(name)
        current += ref -> 0 // a reference is immediately reachable from itself
      case t:TerminalRule =>
        current += t -> 0 // a terminal node is also immediately reachable from itself
      case _ => current ++= computeImmediateSteps(decl)
    }

    var distance = 1
    var newNonTerms = 0
    do {
      val unexplored = current.keys.collect{case Reference(name,_) => name}.filterNot(seen).toList
      val discovery = unexplored.map(transitions).map(_.mapValues(_ + distance))
      newNonTerms = unexplored.size
      seen ++= unexplored

      for {
        discovered <- discovery
        (d, v) <- discovered
      } {
        if (!current.contains(d) || v < current(d)) {
          current(d) = v
        }
      }
      distance += 1

    } while (newNonTerms > 0)

    current
  }

  /** Computes for a declaration all reachable terminals and nonterminals and the number of steps needed to reach them. */
  protected val reachability: Map[DerivationRule, mutable.Map[DerivationRule, Int]] = {
    // phase 1: compute immediate transition map
    val transitionalMap = mutable.HashMap[NonTerminal, Map[DerivationRule, Int]]() withDefault (_ => Map.empty)
    for ((nonTerm, rhs) <- grammar.rules) {
      transitionalMap(nonTerm) = computeImmediateSteps(rhs)
    }
    // phase 2: compute reachability map for all derivation rules
    val tmp = mutable.HashMap[DerivationRule, mutable.Map[DerivationRule, Int]]() withDefault (_ => mutable.HashMap.empty)
    for ((_, rhs) <- grammar.rules) {
      rhs.toStream.foreach { decl =>
        tmp(decl) = reachable(decl, transitionalMap)
      }
    }
    tmp.toMap
  }
}


private[tribble] class KPathCoverage(k: Int, random: Random, val grammar: GrammarRepr) extends Heuristic with ReachabilityInformation  {
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

  override def createdNode(node: DTree): Unit =  node.decl match {
    case ref:Reference =>
//      println(ref)
      usages(getKTuple(ref, node.parent)) += 1
    case t: TerminalRule =>
//      println(t)
      usages(getKTuple(t, node.parent)) += 1
    case _ => // uninteresting
  }

  override protected def makeChoice(q: ListBuffer[Slot]): Slot = {
    case class Selection(score: Double, index: Int)
    val leastConsideredSelections = mutable.ListBuffer[Selection]()

    for ((slot@Slot(decl,_,_), i) <- q.zipWithIndex) {
      val reach = reachability(decl)
      val score: Double = if (reach.nonEmpty) reach.map { case (n, s) => computeScore(n, s, slot) }.min else Double.MaxValue
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

  override def startedTree(): Unit = ()

  override def finishedTree(root: DTree): Unit = ()
}
