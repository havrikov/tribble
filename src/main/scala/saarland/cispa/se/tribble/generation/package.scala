package saarland.cispa.se.tribble

import java.util

import saarland.cispa.se.tribble.model.{DNode, DerivationRule}

import scala.util.Random

package object generation {
  private[generation] case class Slot(decl: DerivationRule, pos: Int, parent: DNode)
  private[generation] object Slot extends ((DerivationRule, Int, DNode) => Slot)

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
}
