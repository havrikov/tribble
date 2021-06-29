package de.cispa.se.tribble
package generation

import org.jgrapht.Graph
import org.jgrapht.alg.shortestpath.{CHManyToManyShortestPaths, EppsteinShortestPathIterator}
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.builder.{GraphBuilder, GraphTypeBuilder}
import org.jgrapht.util.{ConcurrencyUtil, SupplierUtil}

import java.util.{Collections, Map => JMap, Set => JSet}
import scala.collection.JavaConverters._
import scala.collection.mutable


sealed class Reachability(grammar: GrammarRepr) {
  /**
    * This directed unweighted graph represents the derivations of the grammar.
    * The vertices are [[DerivationRule]]s and the edges are possible derivations.
    */
  val grammarGraph: Graph[DerivationRule, DefaultEdge] = Reachability.constructGraph(grammar)

  private val _immediateSuccessors: mutable.Map[DerivationRule, mutable.Set[DerivationRule]] =
    mutable.Map(grammarGraph.vertexSet().asScala.toSeq.map(_ -> mutable.Set[DerivationRule]()): _*)

  private val _reachability: mutable.Map[DerivationRule, mutable.Map[DerivationRule, Int]] =
    mutable.Map(grammarGraph.vertexSet().asScala.toSeq.map(_ -> new mutable.HashMap[DerivationRule, Int].withDefaultValue(Int.MaxValue)): _*)

  // gather interesting rules that are not guaranteed to be reachable from the root
  private val preliminaryTargets = grammarGraph.vertexSet().asScala.filter(isInteresting).toSet
  private val paths = {
    val executor = ConcurrencyUtil.createThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val calc = new CHManyToManyShortestPaths(grammarGraph, executor)
    val paths = calc.getManyToManyPaths(grammarGraph.vertexSet(), preliminaryTargets.asJava)
    executor.shutdown()
    paths
  }

  // Populate the reachability and immediate successors by consulting the shortest paths
  // from all nodes in the graph to all interesting nodes.
  grammarGraph.vertexSet().forEach { s =>
    preliminaryTargets.foreach { t =>
      // There is an edge case if s == t.
      // In this case we must not use the shortest path as given by the CHManyToManyShortestPaths algorithm
      // because it simply reports a path consisting of the one node.
      // Instead we use the EppsteinShortestPathIterator and choose a path that has at least one edge.
      val path = if (s == t) {
        new EppsteinShortestPathIterator(grammarGraph, s, t).asScala.find(_.getLength > 0).orNull
      } else {
        paths.getPath(s, t)
      }

      if (path != null) {
        _reachability(s)(t) = path.getLength
        // if there are no interesting rules between the source and target, the target is immediately reachable
        if (path.getVertexList.asScala.drop(1).reverseIterator.drop(1).forall(!isInteresting(_))) {
          _immediateSuccessors(s).add(t)
        }
      }
    }
  }

  /** The set of derivation rules that are of interest to the current metric. */
  val interestingRules: Set[DerivationRule] = _reachability(grammar.root).keySet.toSet ++ (if (grammar.root.isInstanceOf[Reference]) {
    // Edge case: the root is not reachable from itself by construction,
    // however, it should still count as an interesting rule if it is a Reference.
    Set(grammar.root)
  } else {
    Set()
  })

  /** The set of derivation rules that are of interest to the current metric. */
  def getInterestingRules: JSet[DerivationRule] = Collections.unmodifiableSet(interestingRules.asJava)

  /** For all derivation rules, gives which interesting rules are reachable and after how many derivations at the least. */
  val reachability: Map[DerivationRule, mutable.Map[DerivationRule, Int]] = _reachability.toMap

  /** For all derivation rules, gives which interesting rules are reachable and after how many derivations at the least. */
  def getReachability: JMap[DerivationRule, JMap[DerivationRule, Int]] = {
    Collections.unmodifiableMap(_reachability.mapValues[JMap[DerivationRule, Int]](x => Collections.unmodifiableMap(x.asJava)).asJava)
  }

  /** For all derivation rules, gives which interesting rules are reachable as the next k-path node. */
  val immediateSuccessors: Map[DerivationRule, Set[DerivationRule]] = _immediateSuccessors.mapValues(_.toSet).toMap

  /** For all derivation rules, gives which interesting rules are reachable as the next k-path node. */
  def getImmediateSuccessors: JMap[DerivationRule, JSet[DerivationRule]] = {
    Collections.unmodifiableMap(_immediateSuccessors.mapValues(x => Collections.unmodifiableSet(x.asJava)).asJava)
  }

  /** Indicates whether to consider this rule as a target for reachability. References and Terminals by default. */
  protected def isInteresting(rule: DerivationRule): Boolean = rule match {
    case _: Reference => true
    case _: TerminalRule => true
    case _ => false
  }
}

object Reachability {
  private[tribble] def constructGraph(g: GrammarRepr): Graph[DerivationRule, DefaultEdge] = {
    val emptyGraph: Graph[DerivationRule, DefaultEdge] = GraphTypeBuilder
      .directed()
      .weighted(true)
      .allowingMultipleEdges(false)
      .allowingSelfLoops(false)
      .edgeSupplier(SupplierUtil.DEFAULT_EDGE_SUPPLIER)
      .buildGraph()

    val builder: GraphBuilder[DerivationRule, DefaultEdge, Graph[DerivationRule, DefaultEdge]] = new GraphBuilder(emptyGraph)

    g.rules.values.flatMap(_.toStream).foreach {
      case ref: Reference => builder.addEdge(ref, g(ref))
      case c: Concatenation => c.elements.foreach(builder.addEdge(c, _))
      case a: Alternation => a.alternatives.foreach(builder.addEdge(a, _))
      case q: Quantification => builder.addEdge(q, q.subject)
      case _: TerminalRule =>
    }

    builder.buildAsUnmodifiable()
  }
}

class NonTerminalReachability(grammar: GrammarRepr) extends Reachability(grammar) {

  override protected def isInteresting(rule: DerivationRule): Boolean = rule.isInstanceOf[Reference]
}
