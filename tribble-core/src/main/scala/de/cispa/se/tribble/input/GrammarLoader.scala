package de.cispa.se.tribble
package input

import java.io.File


/**
  * Tries to load the corresponding grammar from the cache, or failing to do so,
  * applies the given loading strategy to acquire the grammar from the given file.
  */
private[tribble] class GrammarLoader(loadingStrategy: LoadingStrategy, grammarCache: GrammarCache) {

  def loadGrammar(grammarFile: File): GrammarRepr = {
    grammarCache.loadGrammar(grammarFile.computeHash()) match {
      case Some(grammar) => grammar
      case None => loadingStrategy.load(grammarFile)
    }
  }
}
