package de.cispa.se.tribble.input

import java.nio.file.{Files, Paths}


private[tribble] trait SharedAutomatonCache {
  private val automatonDir = Files.createDirectories(Paths.get("../build/tmp/test-data/automata"))
  val automatonCache = new AutomatonCache(automatonDir.toFile)
}

private[tribble] trait SharedModelAssembler extends SharedAutomatonCache {
  val modelAssembler = new ModelAssembler(automatonCache, Double.MinPositiveValue, 1.0d)
}
