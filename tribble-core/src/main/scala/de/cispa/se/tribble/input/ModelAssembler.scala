package de.cispa.se.tribble
package input

import java.util.StringJoiner
import de.cispa.se.tribble.model.AutomatonTransformer
import org.log4s.getLogger

import scala.collection.mutable

private[tribble] class ModelAssembler(
                                       automatonCache: AutomatonCache,
                                       damping: Double,
                                       similarity: Double,
                                       transformRegexes: Boolean = false,
                                       mergeLiterals: Boolean = false,
                                       checkDuplicateAlternatives: Boolean = true,
                                     ) {

  def assemble(productions: Seq[Production]): GrammarRepr = {
    val phases = List(
      new BaseAssembly(productions),
      new AutomatonAssembly(automatonCache)) ++
      (if (transformRegexes) List(new RegexTransformation) else Nil) ++
      (if (mergeLiterals) List(new LiteralMerge) else Nil) ++
      (if (checkDuplicateAlternatives) List(CheckDuplicateAlternatives) else Nil) ++
      List(
        new NodeDisambiguation,
        new ShortestDerivationComputation,
        new ProbabilityAssignment,
        new ProbabilityRemapping(damping, similarity),
        GrammarStatistics
      )

    var grammar: GrammarRepr = GrammarRepr("No start symbol set!", Map.empty)
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

class BaseAssembly(productions: Seq[Production]) extends AssemblyPhase {
  val seen = new mutable.HashSet[NonTerminal]

  override def process(grammar: GrammarRepr): GrammarRepr = {
    var g = grammar
    for (production@(_, rhs) <- productions) {
      g = g + production // update grammar while merging redefinitions
      seen ++= rhs.toStream.flatMap { case Reference(n, _) => Some(n) case _ => None } // keep track of references
    }

    // check result
    val unused = g.rules.keySet diff seen
    if (unused.isEmpty) throw new IllegalArgumentException("Grammar contains no root symbol!")
    if (unused.size > 1) throw new IllegalArgumentException(raw"Grammar contains multiple root symbols: ${unused.mkString(", ")}")
    val undefined = seen diff g.rules.keySet
    if (undefined.nonEmpty) throw new IllegalArgumentException(raw"Grammar contains undefined symbols: ${undefined.mkString(", ")}")
    // set the right start symbol
    g.copy(start = unused.head)
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
    } while (grammar.rules.values.flatMap(_.toStream).exists(_.shortestDerivation == Int.MaxValue))
    logger.info(s"Computed shortest derivations in $c iterations.")
    grammar
  }
}

class AutomatonAssembly(automatonCache: AutomatonCache) extends AssemblyPhase {
  private[this] val logger = getLogger

  override def process(grammar: GrammarRepr): GrammarRepr = {
    logger.info("Constructing regex automata...")
    var constructed = 0
    grammar.rules.values.flatMap(_.toStream).foreach {
      case r@Regex(pattern, _) if r.automaton == null =>
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
      case q@Quantification(subject, min, max) =>
        val (s, additions) = expandedRule(subject)
        val newQ = Quantification(s, min, max)
        newQ.probability = q.probability
        newQ -> additions
      case alt@Alternation(alternatives) =>
        val set = alternatives.map(expandedRule)
        val a = set.map(_._1)
        val additions = set.flatMap(_._2)
        val newAlt = Alternation(a)
        newAlt.probability = alt.probability
        newAlt -> additions.toMap
      case conc@Concatenation(elements) =>
        val list = elements.map(expandedRule)
        val e = list.map(_._1)
        val additions = list.flatMap(_._2)
        val newConc = Concatenation(e)
        newConc.probability = conc.probability
        newConc -> additions.toMap
      case r: Reference => r -> Map.empty
      case l: Literal => l -> Map.empty
      case r: Regex =>
        val (name, productions) = AutomatonTransformer.transform(r.automaton, s"r_${transformedAutomata.value}_")
        transformedAutomata.value += 1
        val newRef = Reference(name)
        newRef.probability = r.probability
        newRef -> productions
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
    grammar.copy(rules = updatedProductions.toMap)
  }

}

class LiteralMerge extends AssemblyPhase {
  private[this] val logger = getLogger

  private def merged(rule: DerivationRule)(implicit grammar: GrammarRepr, merges: Box[Int]): DerivationRule = {
    val newRule = rule match {
      case Concatenation(Literal(first, _) :: Literal(second, _) :: tail) =>
        merges.value += 1
        val combined = Literal(first + second)
        if (tail.isEmpty) combined else merged(Concatenation(combined :: tail))
      case Concatenation((lit@Literal(first, _)) :: (ref@Reference(something, _)) :: tail) =>
        // this can lead to unused references
        grammar.get(something) match {
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
    newRule.probability = rule.probability
    newRule
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
      g = grammar.copy(rules = filtered)
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
      case r@Regex(value, _) => r.id = getAndInc(value)
      case ref@Reference(name, _) => ref.id = getAndInc(name)
      case _ =>
    }
    logger.info(s"Disambiguated $hits nodes")
    grammar
  }
}

trait ApproximateDoubleCalc {
  private val precision = 1E-9d

  protected def approxEqual(value: Double, other: Double): Boolean = (value - other).abs < precision
  protected def approxLess(value: Double, other: Double): Boolean = (other - value) > precision
}

class ProbabilityAssignment extends AssemblyPhase with ApproximateDoubleCalc {
  private[this] val logger = getLogger

  private def processRHS(lhs: NonTerminal, rhs: DerivationRule): Unit = rhs match {
    case Alternation(alternatives) =>
      var p = 1.0d
      var unaccounted = mutable.ListBuffer[DerivationRule]()
      for (a <- alternatives) {
        if (a.probability.isNaN)
          unaccounted += a
        else
          p -= a.probability
      }
      if (approxLess(p,0)) throw new IllegalArgumentException(s"The alternatives for $lhs have cumulative probability > 1!")
      if (approxEqual(p,0) && unaccounted.nonEmpty) logger.warn(s"Some un-annotated alternatives for $lhs have probability zero!")
      // uniformly distribute the remaining probability
      unaccounted.foreach(_.probability = p / unaccounted.size)

      // scale up if necessary
      val orderedAlts = alternatives.toList
      val sum = orderedAlts.map(_.probability).sum
      if (approxLess(sum, 1.0d)) {
        logger.warn(s"The alternatives for $lhs have cumulative probability < 1. They will be effectively scaled up proportionately!")
        val factor = 1.0d / sum
        for (a <- orderedAlts)
          a.probability *= factor
      }
      assert(approxEqual(1.0d, alternatives.map(_.probability).sum))
    case _ =>
  }

  private def issueWarnings(lhs: NonTerminal, parent: DerivationRule): Unit = parent match {
    case Alternation(alternatives) => alternatives.foreach(issueWarnings(lhs, _))
    case _ =>
      val annotated = parent.children.filterNot(_.probability.isNaN)
      if (annotated.nonEmpty)
        logger.warn(s"The following elements annotated with probabilities are not direct children of any alternation in rule $lhs. The annotations will have no effect.\n${annotated.mkString("\n")}")
      parent.children.foreach(issueWarnings(lhs, _))
  }

  override def process(grammar: GrammarRepr): GrammarRepr = {
    for ((lhs, rhs) <- grammar.rules) {
      issueWarnings(lhs, rhs)
      for (elem <- rhs.toStream) {
        processRHS(lhs, elem)
      }
    }
    grammar
  }
}

/**
  * Maps probabilities according to p’ := (p + damping) ^similarity^
  */
class ProbabilityRemapping(damping: Double, similarity: Double) extends AssemblyPhase with ApproximateDoubleCalc {
  override def process(grammar: GrammarRepr): GrammarRepr = {
    grammar.rules.values.flatMap(_.toStream).foreach {
      case Alternation(alternatives) =>
        assert(alternatives.forall(!_.probability.isNaN))
        val orderedAlts = alternatives.toList

        assert(approxEqual(orderedAlts.map(_.probability).sum, 1.0d))
        for (a <- orderedAlts) {
          a.probability = math.pow(a.probability + damping, similarity)
        }

        val maxProb2 = orderedAlts.map(_.probability).sum
        for (a <- orderedAlts) {
          a.probability = a.probability / maxProb2
        }

      case _ =>
    }
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

object CheckDuplicateAlternatives extends AssemblyPhase {
  override def process(grammar: GrammarRepr): GrammarRepr = {
    val violations = mutable.ArrayBuffer[(NonTerminal, Seq[DerivationRule])]()
    grammar.rules.foreach {
      case (production, Alternation(alts)) if alts.distinct.size != alts.size => violations += production -> alts
      case _ =>
    }
    if (violations.nonEmpty) {
      val message = new StringJoiner(System.lineSeparator())
      for ((terminal, rules) <- violations) {
        val joiner = new StringJoiner(" | ", s"$terminal: [", "]")
        rules.foreach(r => joiner.add(r.toString))
        message.add(joiner.toString)
      }
      throw new IllegalArgumentException(s"Duplicate alternatives found in $message")
    }
    grammar
  }
}
