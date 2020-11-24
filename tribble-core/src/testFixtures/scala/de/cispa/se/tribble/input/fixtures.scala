package de.cispa.se.tribble.input

import java.nio.file.{Files, Paths}


private[tribble] trait SharedAutomatonCache {
  private val automatonDir = Files.createDirectories(Paths.get("../build/tmp/test-data/automata"))
  val automatonCache = new AutomatonCache(automatonDir.toFile)
}

private[tribble] trait SharedModelAssembler extends SharedAutomatonCache {
  val modelAssembler = new ModelAssembler(automatonCache, assignProbabilities = false)
}

/** Provides a model assembler which ensure that all derivation rules
  * have their id set to [[de.cispa.se.tribble.DerivationRule.DEFAULT_ID]].
  */
private[tribble] trait SharedNoIdModelAssembler extends SharedAutomatonCache {
  val modelAssembler: ModelAssembler = {
    val assembler = new ModelAssembler(automatonCache, checkIds = false, assignProbabilities = false)
    assembler.appendPhase(ResetIds)
    assembler
  }
}
