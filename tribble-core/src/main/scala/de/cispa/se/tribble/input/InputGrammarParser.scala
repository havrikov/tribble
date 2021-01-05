package de.cispa.se.tribble
package input

import fastparse.P

trait InputGrammarParser {
  def grammar[_: P]: P[Seq[Production]]
}
