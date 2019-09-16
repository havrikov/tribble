package saarland.cispa.se.tribble
package input

import org.log4s.getLogger
import saarland.cispa.se.tribble.model._

import scala.collection.mutable

private[tribble] class ModelAssembler(maxRepetitions: Int, automatonCache: AutomatonCache, damping: Double, similarity: Double, transformRegexes: Boolean = false, mergeLiterals: Boolean = false) {

  def assemble(productions: Seq[Production]): GrammarRepr = {
    val phases = List(
      new BaseAssembly(productions, maxRepetitions),
      new AutomatonAssembly(automatonCache)) ++
      (if (transformRegexes) List(new RegexTransformation) else Nil) ++
      (if (mergeLiterals) List(new LiteralMerge) else Nil) ++
      List(
        new NodeDisambiguation,
        new ShortestDerivationComputation,
        GrammarStatistics
      )

    var grammar: GrammarRepr = "No start symbol set!" -> Map.empty
    for (phase <- phases) {
      grammar = phase.process(grammar)
    }
    grammar
  }

  // Possible optimizations:
  // - associativity merge
  // - recursion elimination

}

trait AssemblyPhase {
  def process(grammar: GrammarRepr): GrammarRepr

  protected case class Box[T](var value: T)

}

class BaseAssembly(productions: Seq[Production], maxRepetitions: Int) extends AssemblyPhase {
  val seen = new mutable.HashSet[NonTerminal]

  /** constrains any [[saarland.cispa.se.tribble.model.Quantification]] to be within [min,maxRepetitions] */
  private def constrained(rule: DerivationRule): DerivationRule = rule match {
    case Quantification(subj, min, max) =>
      // TODO this does not constrain anything if min > maxRepetitions. Find out if this is the desired behavior, and if so, produce a warning
      Quantification(constrained(subj), min, Math.max(min, Math.min(max, maxRepetitions)))
    case Alternation(alternatives) =>
      Alternation(alternatives.map(constrained))
    case Concatenation(elements) =>
      Concatenation(elements.map(constrained))
    case x => x
  }

  override def process(grammar: GrammarRepr): GrammarRepr = {
    var g = grammar
    for (production@(_, rhs) <- productions) {
      g = g + production.copy(_2 = constrained(rhs)) // update grammar while merging redefinitions
      seen ++= rhs.toStream.flatMap { case Reference(n, _) => Some(n) case _ => None } // keep track of references
    }

    // check result
    val unused = g.rules.keySet diff seen
    if (unused.isEmpty) throw new IllegalArgumentException("Grammar contains no root symbol!")
    if (unused.size > 1) throw new IllegalArgumentException(raw"Grammar contains multiple root symbols: ${unused.mkString(", ")}")
    val undefined = seen diff g.rules.keySet
    if (undefined.nonEmpty) throw new IllegalArgumentException(raw"Grammar contains undefined symbols: ${undefined.mkString(", ")}")
    // set the right start symbol
    g.copy(_1 = unused.head)
  }
}

class ShortestDerivationComputation extends AssemblyPhase {
  private[this] val logger = getLogger

  private def incNoOverflow(value: Int): Int = if (value >= Int.MaxValue - 1) Int.MaxValue else value + 1

  private def resolve(rule: DerivationRule)(implicit resolved: mutable.Map[NonTerminal, Int], grammar: GrammarRepr): Int = {
    rule match {
      case r@Reference(name, _) =>
        r.shortestDerivation = incNoOverflow(resolved.getOrElse(name, Int.MaxValue))
      case c@Concatenation(elements) =>
        c.shortestDerivation = incNoOverflow(elements.map(resolve).max)
      case a@Alternation(alternatives) =>
        a.shortestDerivation = incNoOverflow(alternatives.map(resolve).min)
      case q@Quantification(subject, min, _) =>
        q.shortestDerivation = if (min == 0) 0 else incNoOverflow(resolve(subject))
      case _ =>
    }
    rule.shortestDerivation
  }


  private def updateShortestDerivation(prod: Production)(implicit resolved: mutable.Map[NonTerminal, Int], grammar: GrammarRepr): Unit = {
    val (nonterm, rule) = prod
    val min = rule.toStream.minBy(resolve).shortestDerivation
    if (min < Int.MaxValue)
      resolved(nonterm) = rule.shortestDerivation
  }

  override def process(grammar: GrammarRepr): GrammarRepr = {
    val storage = new mutable.HashMap[NonTerminal, Int]()
    var c = 1
    do {
      grammar.rules.foreach(updateShortestDerivation(_)(storage, grammar))
      c += 1
    } while (grammar.map(_.shortestDerivation).contains(Int.MaxValue))
    logger.info(s"Computed shortest derivations in $c iterations.")
    grammar
  }
}

class AutomatonAssembly(automatonCache: AutomatonCache) extends AssemblyPhase {
  private[this] val logger = getLogger

  override def process(grammar: GrammarRepr): GrammarRepr = {
    logger.info("Constructing regex automata...")
    var constructed = 0
    grammar.foreach {
      case r@Regex(pattern, null, _) =>
        r.automaton = automatonCache.getAutomaton(pattern)
        constructed += 1
      case _ =>
    }
    logger.info(s"Constructed $constructed automata.")
    grammar
  }
}

class RegexTransformation extends AssemblyPhase {
  private[this] val logger = getLogger

  private def expandedRule(rule: DerivationRule)(implicit transformedAutomata: Box[Int]): (DerivationRule, Map[NonTerminal, DerivationRule]) = {
    val expanded: (DerivationRule, Map[NonTerminal, DerivationRule]) = rule match {
      case Quantification(subject, min, max) =>
        val (s, additions) = expandedRule(subject)
        Quantification(s, min, max) -> additions
      case Alternation(alternatives) =>
        val set = alternatives.map(expandedRule)
        val a = set.map(_._1)
        val additions = set.flatMap(_._2)
        Alternation(a) -> additions.toMap
      case Concatenation(elements) =>
        val list = elements.map(expandedRule)
        val e = list.map(_._1)
        val additions = list.flatMap(_._2)
        Concatenation(e) -> additions.toMap
      case r: Reference => r -> Map.empty
      case l: Literal => l -> Map.empty
      case Regex(_, automaton, _) =>
        val (name, productions) = AutomatonTransformer.transform(automaton, s"r_${transformedAutomata.value}_")
        transformedAutomata.value += 1
        Reference(name) -> productions
    }
    expanded
  }

  override def process(grammar: GrammarRepr): GrammarRepr = {
    logger.info("Transforming automata into productions...")
    implicit val transformedAutomata: Box[Int] = Box(0)

    // keep outstanding changes
    val updatedProductions = mutable.Map[NonTerminal, DerivationRule]()

    grammar.rules.foreach { case (nonterm, rule) =>
      val (newRule, productions) = expandedRule(rule)
      updatedProductions(nonterm) = newRule
      updatedProductions ++= productions
    }
    // might need to force the view

    logger.info(s"Transformed ${transformedAutomata.value} automata into productions.")
    grammar.copy(_2 = updatedProductions.toMap)
  }

}

class LiteralMerge extends AssemblyPhase {
  private[this] val logger = getLogger

  private def merged(rule: DerivationRule)(implicit grammar: GrammarRepr, merges: Box[Int]): DerivationRule = rule match {
    case Concatenation(Literal(first, _) :: Literal(second, _) :: tail) =>
      merges.value += 1
      val combined = Literal(first + second)
      if (tail.isEmpty) combined else merged(Concatenation(combined :: tail))
    case Concatenation((lit@Literal(first, _)) :: (ref@Reference(something, _)) :: tail) =>
      // this can lead to unused references
      grammar(something) match {
        case Literal(second, _) =>
          merges.value += 1
          val combined = Literal(first + second)
          if (tail.isEmpty) combined else merged(Concatenation(combined :: tail))
        case _ => Concatenation(lit :: ref :: tail.map(merged))
      }
    case Concatenation(elements) => Concatenation(elements.map(merged))
    case Quantification(subject, min, max) => Quantification(merged(subject), min, max)
    case Alternation(alternatives) => Alternation(alternatives.map(merged))
    case _ => rule
  }

  override def process(grammar: GrammarRepr): GrammarRepr = {
    val merges = Box(0)
    var removed = 0
    var g = grammar
    var done = false
    do {
      val mergedProductions = g.rules.mapValues(merged(_)(g, merges)).view.force // force eager execution
      // remove unused references
      val usedReferences = mergedProductions.values.flatMap(_.toStream.collect { case Reference(n, _) => n }).toSet + g.start
      val filtered = mergedProductions.filterKeys(usedReferences)
      removed += mergedProductions.size - filtered.size
      done = filtered == g.rules
      g = grammar.copy(_2 = filtered)
    } while (!done)

    logger.info(s"Merged ${merges.value} literals${if (removed > 0) s" (and removed $removed productions in the process)" else ""}")
    g
  }
}

class NodeDisambiguation extends AssemblyPhase {
  private[this] val logger = getLogger
  private val ids = mutable.Map[String, Int]() withDefaultValue 0
  private var hits = 0

  private def getAndInc(key: String): Int = {
    val r = ids(key)
    if (r != 0) hits += 1 // log a hit
    ids(key) += 1
    r
  }

  override def process(grammar: GrammarRepr): GrammarRepr = {
    // cannot use grammar.foreach because at this point all terminals with equal strings have an id of 0 and are thus considered equal, and therefore multiple occurrences get filtered out
    grammar.rules.values.flatMap(_.toStream).foreach {
      case l@Literal(value, _) => l.id = getAndInc(value)
      case r@Regex(value, _, _) => r.id = getAndInc(value)
      case ref@Reference(name, _) => ref.id = getAndInc(name)
      case _ =>
    }
    logger.info(s"Disambiguated $hits nodes")
    grammar
  }
}

object GrammarStatistics extends AssemblyPhase {
  private val logger = getLogger

  override def process(grammar: GrammarRepr): GrammarRepr = {
    val nodes = grammar.rules.values.flatMap(_.toStream).size
    logger.info(s"The Grammar has ${grammar.rules.size} productions and $nodes nodes")
    grammar
  }
}
