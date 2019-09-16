package saarland.cispa.se.tribble
package generation

import saarland.cispa.se.tribble.model.DTree

trait ForestGenerator {
  def generateForest(): Iterable[DTree]
}
