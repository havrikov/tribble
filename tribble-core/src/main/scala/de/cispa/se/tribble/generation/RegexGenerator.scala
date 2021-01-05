package de.cispa.se.tribble
package generation

import dk.brics.automaton.{Automaton, State}

import scala.annotation.tailrec
import scala.util.Random

private[tribble] class RegexGenerator(random: Random, minLength: Int) {

  @tailrec
  private def generateString(state: State)(implicit builder: StringBuilder): Unit = {
    val transitions = state.getTransitions
    // carry on if we are not in an accepting state or there is still input to generate to reach minLength
    if (!state.isAccept || !transitions.isEmpty && !(builder.length >= minLength && random.nextBoolean())) {
      val transition = choose(transitions)(random)
      val range = transition.getMin to transition.getMax
      val res = range(random.nextInt(range.size)) // todo check for overflow when min = 0 and max = Int.MaxValue
      builder.append(res)
      generateString(transition.getDest)
    }
  }

  private[tribble] def generateIntoBuilder(automaton: Automaton, builder: StringBuilder): StringBuilder = {
    generateString(automaton.getInitialState)(builder)
    builder
  }

}
