package de.cispa.se.tribble
package output

import de.cispa.se.tribble.model.ParentTracker

import scala.annotation.tailrec

class KPathEvaluator(private val parentTracker: ParentTracker) {

  def evaluateKPath(kPath: Seq[DerivationRule]): DerivationRule = {
    checkKPath(kPath)
    var current: DerivationRule = kPath.last
    for (elem <- kPath.reverseIterator) {
      val context = focusedParent(elem)
      current = substituteRule(context, elem, current)
    }
    current
  }

  /**
    * Returns a predecessor derivation rule which contains the given decl,
    * such that there are no alternatives on the way from this predecessor to decl.
    *
    * If decl is toplevel, it is returned itself.
    */
  @tailrec
  private def focusedParent(decl: DerivationRule): DerivationRule = {
    parentTracker.parent(decl) match {
      case Some(p) => p match {
        case _: Alternation =>
          decl
        case Concatenation(elements, id) =>
          // strip out nullable quantifications
          val rules = elements.filterNot(rule => rule != decl && rule.isInstanceOf[Quantification] && rule.asInstanceOf[Quantification].min == 0)
          if (rules.size == 1) {
            rules.head
          } else {
            Concatenation(rules, id)
          }
        case _ =>
          focusedParent(p)
      }
      case None => decl
    }
  }

  /** Replaces an element in the given rule with the given replacement */
  private def substituteRule(where: DerivationRule, target: DerivationRule, replacement: DerivationRule): DerivationRule = {
    // if the location is just the target itself, replace it completely
    if (where == target) {
      replacement
    } else {
      where match {
        // the .flatMap(unwrapConcat) resolves things like a ~ (b ~ c) into a ~ b ~ c
        case Concatenation(elements, id) => Concatenation(elements.map(substituteRule(_, target, replacement)).flatMap(unwrapConcat), id)
        case Quantification(subject, min, max, id) => Quantification(substituteRule(subject, target, replacement), min, max, id)
        case _ => where
      }
    }
  }

  private def unwrapConcat(rule: DerivationRule): Seq[DerivationRule] = rule match {
    case Concatenation(elements, _) => elements
    case _ => rule :: Nil
  }

  /**
    * Ensures the kPath is possible according to the grammar at hand
    *
    * @throws IllegalArgumentException if the given kPath is impossible
    */
  private def checkKPath(kPath: Seq[DerivationRule]): Unit = {
    require(kPath.nonEmpty)
    require(kPath.take(kPath.size - 1).forall(_.isInstanceOf[Reference]), "A k-path can only contain References in all but the last position!")
    require(kPath.last.isInstanceOf[Reference] || kPath.last.isInstanceOf[TerminalRule], "A k-path must end in either a Reference or a TerminalRule!")
    // TODO implement actual "reachability" check
    // idea: the reverse kPath should be contained in the ancestor path of the last element of kPath
  }
}
