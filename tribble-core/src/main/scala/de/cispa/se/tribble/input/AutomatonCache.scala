package de.cispa.se.tribble
package input

import java.io.{File, FileInputStream, FileOutputStream}

import dk.brics.automaton.{Automaton, RegExp}
import org.log4s.getLogger

import scala.collection.mutable

private[tribble] class AutomatonCache(storageDir: File) {
  require(storageDir.isDirectory, s"The persistent automaton storage must be a directory! (Given $storageDir)")
  require(storageDir.exists(), "The persistent automaton storage must exist!")
  private val logger = getLogger

  logger.debug(s"Reading stored automata from $storageDir")
  private val storage: mutable.Map[String, Automaton] = mutable.Map(storageDir.listFiles()
    .filter(_.isFile)
    .filter(_.getName.endsWith(".aut"))
    .map(f => f.getName -> Automaton.load(new FileInputStream(f))): _*)
  logger.info(s"""Read ${storage.size} automata from persistent storage "$storageDir"""")


  def getAutomaton(regex: String): Automaton = {
    val key = generateName(regex)
    if (storage.contains(key)) {
      logger.trace(s"""Accessing cached automaton for "$regex"""")
      storage(key)
    } else {
      logger.debug(s"""Creating a new automaton for "$regex"""")
      val automaton = new RegExp(regex, RegExp.COMPLEMENT | RegExp.INTERSECTION).toAutomaton
      automaton.removeDeadTransitions()
      storage(key) = automaton
      val targetFile = new File(storageDir, key)
      logger.debug(s"""Persisting the automaton for "$regex" in $targetFile""")
      automaton.store(new FileOutputStream(targetFile))
      automaton
    }
  }

  /**
    * Generates a filesystem-compatible name from a regex
    */
  private def generateName(regex: String): String = {
    s"${hexEncode(regex)}.aut"
  }

  /**
    * Hex encode a string
    * taken from https://stackoverflow.com/a/2149927
    */
  private def hexEncode(arg: String): String = {
    f"${new java.math.BigInteger(1, arg.getBytes("UTF-8"))}%040x"
  }

}
