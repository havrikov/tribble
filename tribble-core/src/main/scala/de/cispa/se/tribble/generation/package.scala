package de.cispa.se.tribble

import java.util

import scala.util.Random

package object generation {
  private[tribble] case class Slot(decl: DerivationRule, pos: Int, parent: DNode)
  private[tribble] object Slot extends ((DerivationRule, Int, DNode) => Slot)

  @inline
  def choose[A](set: util.Set[A])(implicit random: Random): A = {
    // this method should be faster than first converting the set into a scala set and using `drop(n)` on its iterator
    val n = random.nextInt(set.size)
    val it = set.iterator()
    var i = 1
    while (i < n) {
      it.next()
      i += 1
    }
    it.next
  }

  def minimalElementsBy[A, B](list: Seq[A], f: A => B)(implicit cmp: Ordering[B]): Seq[A] = {
    if (list.isEmpty)
      throw new UnsupportedOperationException("empty.minimalElementsBy")
    val minElement = list.minBy(f)
    val minValue = f(minElement)
    list filter { x => cmp.equiv(f(x), minValue) }
  }
}
