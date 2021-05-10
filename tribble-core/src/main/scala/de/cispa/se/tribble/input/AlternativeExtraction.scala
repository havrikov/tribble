package de.cispa.se.tribble
package input

import scala.collection.mutable

/**
  * Extracts all internal alternatives into top level. <br>
  * <b>Run [[AssignIds]] afterwards to give newly created rules unique ids!</b>
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A | 'B,
  * 'A := "a1" ~ ('D | "a2").? ~ ("a3" ~ ('D | "a4" | "a5")).rep,
  * 'B := "b1" | "b2" ~ ("b3" ~ ('D | 'C ~ "b4") | 'C),
  * 'C := "c1" | "c2" ~ ("c3" | "c4"),
  * 'D := "d1" | "d2"
  * </pre>
  *
  * will be transformed into
  *
  * <pre>
  * 'S := 'A | 'B,
  * 'A := "a1" ~ 'A_a0.? ~ ("a3" ~ 'A_a1).rep,
  * 'A_a0 := 'D | "a2",
  * 'A_a1 :=  'D | "a4" | "a5",
  * 'B := "b1" | "b2" ~ 'B_a0,
  * 'B_a0 := "b3" ~ 'B_a0_a0 | 'C,
  * 'B_a0_a0 := 'D | 'C ~ "b4",
  * 'C := "c1" | "c2" ~ 'C_a0,
  * 'C_a0 := "c3" | "c4",
  * 'D := "d1" | "d2"
  * </pre>
  *
  */
object AlternativeExtraction extends AssemblyPhase {

  private type Grammar = mutable.Map[NonTerminal, DerivationRule]

  override def process(grammar: GrammarRepr): GrammarRepr = {
    val newRules: Grammar = mutable.Map[NonTerminal, DerivationRule]()
    val ids = mutable.Map[NonTerminal, Int]() withDefaultValue 0
    for ((nonTerminal, rule) <- grammar.rules) {
      outerExtract(nonTerminal, rule)(newRules, ids)
    }
    GrammarRepr(grammar.start, rules = newRules.toMap)
  }

  /**
    * Extracts alternatives from a top-level grammar production.
    * This method will NOT create new references for alternations encountered on top level.
    * Side effect: updates the grammar with newly created sub-rules.
    *
    * @return the possibly rewritten derivation rule for the given nonTerminal.
    */
  private def outerExtract(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    case Concatenation(elements, id) =>
      val c = Concatenation(elements.map(innerExtract(nonTerminal, _)), id)
      grammar(nonTerminal) = c
      c
    case a: Alternation =>
      val newA = Alternation(a.alternatives.map(innerExtract(nonTerminal, _)), a.id)
      grammar(nonTerminal) = newA
      newA
    case Quantification(subject, min, max, id) =>
      val q = Quantification(innerExtract(nonTerminal, subject), min, max, id)
      grammar(nonTerminal) = q
      q
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }

  /**
    * Extracts alternatives from an internal derivation rule.
    * This method will create new references for all encountered alternations.
    * Side effect: updates the grammar with newly created sub-rules.
    *
    * @return the possibly rewritten derivation rule for the given nonTerminal.
    */
  private def innerExtract(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    case Concatenation(elements, id) =>
      val c = Concatenation(elements.map(innerExtract(nonTerminal, _)), id)
      grammar(nonTerminal) = c
      c
    case a: Alternation =>
      val index = ids(nonTerminal)
      ids(nonTerminal) += 1
      val name = s"${nonTerminal}_a$index"
      val r = Reference(name)
      grammar(nonTerminal) = r
      val newA = Alternation(a.alternatives.map(innerExtract(name, _)))
      grammar(name) = newA
      r
    case Quantification(subject, min, max, id) =>
      val q = Quantification(innerExtract(nonTerminal, subject), min, max, id)
      grammar(nonTerminal) = q
      q
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }
}
