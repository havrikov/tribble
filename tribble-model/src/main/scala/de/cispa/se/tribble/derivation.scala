package de.cispa.se.tribble

import scala.annotation.tailrec
import scala.collection.mutable

/*
* There was an idea to have separate genotypes and phenotypes.
* The genotype would be an abstract derivation tree without concrete regex values,
* while the phenotype would have those instantiated to concrete values.
*
* This was deemed too complex and the benefits were unclear.
* */

sealed trait DTree {

  def decl: DerivationRule

  def parent: Option[DNode]

  def isLeaf: Boolean

  def isRoot: Boolean = parent.isEmpty

  @tailrec
  final def root: DTree = {
    if (parent.isDefined) {
      this
    } else {
      parent.get.asInstanceOf[DTree].root
    }
  }

  def size(): Int = {
    @tailrec
    def helper(agenda: mutable.ListBuffer[DTree], acc: Int): Int = agenda.length match {
      case 0 => acc
      case _ => agenda.remove(0) match {
        case _: DLeaf =>
          helper(agenda, acc + 1)
        case DNode(_, _, children) =>
          agenda.prependAll(children.valuesIterator)
          helper(agenda, acc + 1)
      }
    }

    helper(mutable.ListBuffer(this), 0)
  }

  /** @return the maximal depth of the subtree rooted at this node */
  def depth(): Int

  def nodes: List[DNode]

  def leaves: Iterable[DLeaf]

  def deepCopy(): DTree = _copy(None)
  protected[tribble] def _copy(newParent: Option[DNode]): DTree

  def dfs[U](f: DTree => U): Unit
}

final case class DNode(decl: DerivationRule, parent: Option[DNode], children: mutable.SortedMap[Int, DTree] = mutable.TreeMap.empty) extends DTree {
  // Manual overload for Java compatibility
  def this(decl: DerivationRule, parent: Option[DNode]) = this(decl, parent, mutable.TreeMap.empty)

  def revMap(): mutable.Map[DTree, Int] = children map (_.swap)

  override def isLeaf: Boolean = false

  override def leaves: Iterable[DLeaf] = children.values.flatMap(_.leaves)

  override def toString: String = s"""$decl${children.values.map(_.toString).mkString("[",", ","]")}"""

  override def equals(obj: scala.Any): Boolean = obj match {
    case DNode(d,_,c) => d == decl && c == children
    case _ => false
  }

  override def hashCode(): Int = (decl, children).##

  override protected[tribble] def _copy(newParent: Option[DNode]): DTree = {
    val c = this.copy(parent=newParent, children=mutable.TreeMap.empty)
    val position = parent.fold(newParent.fold(0)(_.children.size))(_.revMap()(this))
    newParent.foreach(_.children(position) = c)
    children.values.foreach(_._copy(Some(c)))
    c
  }

  override def nodes: List[DNode] = this :: children.values.toList.flatMap(_.nodes)

  override def depth(): Int = {
    var depth = 1
    if (children.nonEmpty) {
      depth += children.values.map(_.depth()).max
    }
    depth
  }

  override def dfs[U](f: DTree => U): Unit = {
    f(this)
    children.values.foreach(_.dfs(f))
  }
}

final case class DLeaf(decl: TerminalRule, parent: Option[DNode], value: String) extends DTree {
  override def isLeaf: Boolean = true

  override def leaves: Seq[DLeaf] = this :: Nil

  override def toString: String = value

  override def equals(obj: scala.Any): Boolean = obj match {
    case DLeaf(d,_,v) => d == decl && v == value
    case _ => false
  }

  override def hashCode(): Int = (decl, value).##

  override protected[tribble] def _copy(newParent: Option[DNode]): DTree = {
    val position = parent.fold(newParent.fold(0)(_.children.size))(_.revMap()(this))
    val leaf = copy(parent = newParent)
    newParent.foreach(_.children(position) = leaf)
    leaf
  }

  override def nodes: List[DNode] = Nil

  override val depth = 0

  override def dfs[U](f: DTree => U): Unit = f(this)
}
