package de.cispa.se.tribble

import java.io.File

private[tribble] object Internal {
  /** Produces a hash of the given grammar file to use with a grammar cache. */
  def grammarHash(grammarFile: File): String = grammarFile.computeHash()
}
