package de.cispa.se.tribble
package generation

import org.jgrapht.Graph
import org.jgrapht.alg.shortestpath.EppsteinShortestPathIterator
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.builder.{GraphBuilder, GraphTypeBuilder}
import org.jgrapht.util.SupplierUtil

import java.util.{Map => JMap, Set => JSet}
import scala.collection.JavaConverters._
import scala.collection.mutable


class Reachability(private val grammar: GrammarRepr) {
  /**
    * This directed unweighted graph represents the derivations of the grammar.
    * The vertices are [[DerivationRule]]s and the edges are possible derivations.
    */
  val grammarGraph: Graph[DerivationRule, DefaultEdge] = {
    val emptyGraph: Graph[DerivationRule, DefaultEdge] = GraphTypeBuilder
      .directed()
      .weighted(true)
      .allowingMultipleEdges(false)
      .allowingSelfLoops(false)
      .edgeSupplier(SupplierUtil.DEFAULT_EDGE_SUPPLIER)
      .buildGraph()

    val builder: GraphBuilder[DerivationRule, DefaultEdge, Graph[DerivationRule, DefaultEdge]] = new GraphBuilder(emptyGraph)

    grammar.rules.values.flatMap(_.toStream).foreach {
      case ref: Reference => builder.addEdge(ref, grammar(ref))
      case c: Concatenation => c.elements.foreach(builder.addEdge(c, _))
      case a: Alternation => a.alternatives.foreach(builder.addEdge(a, _))
      case q: Quantification => builder.addEdge(q, q.subject)
      case _: TerminalRule =>
    }

    builder.buildAsUnmodifiable()
  }

  /** The set of derivation rules that are of interest to the current metric. */
  val interestingRules: Set[DerivationRule] = grammarGraph.vertexSet().asScala.filter(isInteresting).toSet

  /** The set of derivation rules that are of interest to the current metric. */
  def getInterestingRules: JSet[DerivationRule] = interestingRules.asJava

  private val _immediateSuccessors: mutable.Map[DerivationRule, mutable.Set[DerivationRule]] =
    mutable.Map(grammarGraph.vertexSet().asScala.toSeq.map(_ -> mutable.Set[DerivationRule]()): _*)

  private val _reachability: mutable.Map[DerivationRule, mutable.Map[DerivationRule, Int]] =
    mutable.Map(grammarGraph.vertexSet().asScala.toSeq.map(_ -> new mutable.HashMap[DerivationRule, Int].withDefaultValue(Int.MaxValue)): _*)


  // Populate the reachability and immediate successors by consulting the shortest paths
  // from all nodes in the graph to all interesting nodes.
  grammarGraph.vertexSet().forEach { s =>
    interestingRules.foreach { t =>
      // We look for two shortest paths and choose the one that has at least one edge
      // because when source and target are the same, jgrapht tends to report a path
      // consisting of the node itself disregarding the fact that there is no self-loop.
      val path = new EppsteinShortestPathIterator(grammarGraph, s, t).asScala.take(2).find(_.getLength > 0).orNull

      if (path != null) {
        _reachability(s)(t) = path.getLength
        // if there are no interesting rules between the source and target, the target is immediately reachable
        if (path.getVertexList.asScala.drop(1).reverseIterator.drop(1).forall(!isInteresting(_))) {
          _immediateSuccessors(s).add(t)
        }
      }
    }
  }

  /** For all derivation rules, gives which interesting rules are reachable and after how many derivations at the least. */
  val reachability: Map[DerivationRule, mutable.Map[DerivationRule, Int]] = _reachability.toMap

  /** For all derivation rules, gives which interesting rules are reachable and after how many derivations at the least. */
  def getReachability: JMap[DerivationRule, JMap[DerivationRule, Int]] = _reachability.mapValues(_.asJava).asJava

  /** For all derivation rules, gives which interesting rules are reachable as the next k-path node. */
  val immediateSuccessors: Map[DerivationRule, Set[DerivationRule]] = _immediateSuccessors.mapValues(_.toSet).toMap

  /** For all derivation rules, gives which interesting rules are reachable as the next k-path node. */
  def getImmediateSuccessors: JMap[DerivationRule, JSet[DerivationRule]] = _immediateSuccessors.mapValues(_.asJava).asJava

  /** Indicates whether to consider this rule as a target for reachability. References and Terminals by default. */
  protected def isInteresting(rule: DerivationRule): Boolean = rule match {
    case _: Reference => true
    case _: TerminalRule => true
    case _ => false
  }
}

class NonTerminalReachability(grammar: GrammarRepr) extends Reachability(grammar) {

  override protected def isInteresting(rule: DerivationRule): Boolean = rule.isInstanceOf[Reference]
}
