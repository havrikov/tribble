package saarland.cispa.se

import saarland.cispa.se.tribble.model._

import scala.collection.mutable

package object tribble {
  type NonTerminal = String
  type Production = (NonTerminal, DerivationRule)
  type GrammarRepr = (NonTerminal, Map[NonTerminal, DerivationRule])
  type Forest = Set[DTree]

  implicit class RichGrammar(val grammar: GrammarRepr) extends AnyVal {
    def start: NonTerminal = grammar._1
    def rules: Map[NonTerminal, DerivationRule] = grammar._2
    def root: DerivationRule = rules(start)
    def pprint: String = s"--> $start\n${rules.map { case (name, sym) => raw"$name ::= $sym;" }.mkString("\n")}"

    /** @return the grammar created by adding the given production */
    def +(production: Production): GrammarRepr = {
      val (lhs, _) = production
      if (grammar.rules.contains(lhs))
        throw new IllegalArgumentException(s"Cannot have multiple declarations for $lhs!")
      grammar.copy(_2 = rules + production)
    }

    def foreach[U](f: DerivationRule => U): Unit = {
      val seen = mutable.HashSet[DerivationRule]()
      val agenda = mutable.ListBuffer(root)
      while (agenda.nonEmpty) {
        val current = agenda.remove(0)
        if (!seen(current)) {
          seen += current
          f(current)
          current match {
            case Reference(name, _) => agenda.prepend(rules(name))
            case Alternation(children) => agenda.prepend(children.toSeq: _*)
            case Concatenation(children) => agenda.prepend(children: _*)
            case Quantification(subj, _, _) => agenda.prepend(subj)
            case _ =>
          }
        }
      }
    }

    def map[U](f: DerivationRule => U): Seq[U] = {
      val res = mutable.ListBuffer[U]()
      foreach(res += f(_))
      res.toList
    }

    def flatMap[U](f: DerivationRule => Iterable[U]): Seq[U] = {
      val res = mutable.ListBuffer[U]()
      foreach(res ++= f(_))
      res.toList
    }

    def get(reference: Reference): Option[DerivationRule] = rules.get(reference.name)
    def get(nonTerminal: NonTerminal): Option[DerivationRule] = rules.get(nonTerminal)
    def apply(reference: Reference): DerivationRule = rules(reference.name)
    def apply(nonTerminal: NonTerminal): DerivationRule = rules(nonTerminal)
  }

}
