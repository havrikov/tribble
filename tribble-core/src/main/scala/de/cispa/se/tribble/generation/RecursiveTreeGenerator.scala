package de.cispa.se.tribble
package generation

abstract class RecursiveTreeGenerator(protected val regexGenerator: RegexGenerator) extends TreeGenerator {

  override def generate(implicit grammar: GrammarRepr): DTree = {
    startGenerate()
    val tree = gen(grammar.root, None, 0)
    finishGenerate(tree)
    tree
  }

  private[tribble] final def gen(decl: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    startNode(decl, parent, currentDepth)
    val tree = alternativeGenerationMethod(decl, parent, currentDepth).getOrElse {
      decl match {
        case ref: Reference => instantiateReference(ref, parent, currentDepth)
        case r: Regex => instantiateRegex(r, parent, currentDepth)
        case l: Literal => instantiateLiteral(l, parent, currentDepth)
        case a: Alternation => instantiateAlternation(a, parent, currentDepth)
        case c: Concatenation => instantiateConcatenation(c, parent, currentDepth)
        case q: Quantification => instantiateQuantification(q, parent, currentDepth)
      }
    }
    finishNode(tree)
    tree
  }

  protected def startGenerate(): Unit = ()

  protected def finishGenerate(tree: DTree): Unit = ()

  protected def startNode(decl: DerivationRule, parent: Option[DNode], currentDepth: Int): Unit = ()

  protected def prepareNode(tree: DTree): Unit = ()

  protected def finishNode(tree: DTree): Unit = ()

  protected def alternativeGenerationMethod(decl: DerivationRule, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): Option[DTree] = None

  protected def instantiateReference(ref: Reference, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val node = DNode(ref, parent)
    prepareNode(node)
    node.children(0) = gen(grammar(ref), Some(node), currentDepth + 1)
    node
  }

  protected def instantiateConcatenation(concatenation: Concatenation, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val node = DNode(concatenation, parent)
    prepareNode(node)
    val trees = concatenation.elements.map(gen(_, Some(node), currentDepth + 1))
    node.children ++= trees.indices zip trees
    node
  }

  protected def instantiateAlternation(alternation: Alternation, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree

  protected def instantiateQuantification(quantification: Quantification, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree

  protected def instantiateLiteral(l: Literal, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val leaf = DLeaf(l, parent, l.value)
    prepareNode(leaf)
    leaf
  }

  protected def instantiateRegex(r: Regex, parent: Option[DNode], currentDepth: Int)(implicit grammar: GrammarRepr): DTree = {
    val leaf = DLeaf(r, parent, regexGenerator.generateIntoBuilder(r.automaton, new StringBuilder()).mkString)
    prepareNode(leaf)
    leaf
  }
}
