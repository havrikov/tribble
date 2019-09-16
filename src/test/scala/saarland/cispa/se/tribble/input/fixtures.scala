package saarland.cispa.se.tribble.input

import java.io.File
import java.nio.file.{Files, Paths}


private[tribble] trait SharedAutomatonCache {
  private val automatonDir = Files.createDirectories(Paths.get("automata"))
  val automatonCache = new AutomatonCache(new File(automatonDir.toString))
}

private[tribble] trait SharedModelAssembler extends SharedAutomatonCache {
  val modelAssembler = new ModelAssembler(10, automatonCache, Double.MinPositiveValue, 1.0d)
}
