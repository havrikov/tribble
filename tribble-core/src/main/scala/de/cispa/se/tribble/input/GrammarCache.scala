package de.cispa.se.tribble
package input

import java.io._
import org.log4s.getLogger


trait GrammarCache {
  def loadGrammar (grammarHash: String): Option[GrammarRepr]
  def storeGrammar(grammar: GrammarRepr, grammarHash: String): Unit
}

/**
  * This grammar cache uses the standard `Serializable` mechanism from Java.
  */
private[tribble] class ObjectStreamGrammarCache(private val storageDir: File) extends GrammarCache {
  require(storageDir.isDirectory, s"The grammar cache must be a directory! (Given $storageDir)")
  require(storageDir.exists(), "The grammar cache directory must exist!")
  private[this] val logger = getLogger

  override def loadGrammar(grammarHash: String): Option[GrammarRepr] = {
    require(grammarHash != null, "The grammar hash must not be null!")
    require(grammarHash.nonEmpty, "The grammar hash must not be empty!")

    val expectedName = s"${grammarHash}.grammar"
    val file = new File(storageDir, expectedName)
    if (file.exists) {
      logger.info("Found cached grammar")
      val ois = new ObjectInputStream(new FileInputStream(file))
      try {
        val grammar: GrammarRepr = ois.readObject().asInstanceOf[GrammarRepr]
        Some(grammar)
      } catch {
        case t: Throwable =>
          logger.error(t)(s"Could not deserialize grammar $expectedName")
          None
      } finally {
        ois.close()
      }
    } else {
      logger.info(s"Found no cached grammar")
      None
    }
  }

  override def storeGrammar(grammar: GrammarRepr, grammarHash: String): Unit = {
    require(grammarHash != null, "The grammar hash must not be null!")
    require(grammarHash.nonEmpty, "The grammar hash must not be empty!")

    val file = new File(storageDir, s"$grammarHash.grammar")
    if (file.exists()) {
      logger.warn("Overriding grammar cache file!")
    }
    val os = new ObjectOutputStream(new FileOutputStream(file))
    try {
      // make sure we have the fully materialized object here and not just a lazy view
      os.writeObject(grammar.copy(rules = grammar.rules.view.force))
    } finally {
      os.close()
    }
    logger.info(s"Stored grammar cache to $file")
  }
}

object EmptyGrammarCache extends GrammarCache {
  override def loadGrammar(grammarHash: String): Option[GrammarRepr] = None

  override def storeGrammar(grammar: GrammarRepr, grammarHash: String): Unit = ()
}
