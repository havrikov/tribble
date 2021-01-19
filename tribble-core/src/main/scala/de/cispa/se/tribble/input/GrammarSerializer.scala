package de.cispa.se.tribble.input

import java.io.{File, _}

import better.files._
import de.cispa.se.tribble.GrammarRepr

object GrammarSerializer {
  def serializeGrammar(grammar: GrammarRepr, file: File): Unit =
  // make sure we have the fully materialized object here and not just a lazy view
    using(new ObjectOutputStream(file.toScala.newOutputStream())) {
      _.writeObject(GrammarRepr(grammar.start, grammar.rules.view.force))
    }

  def deserializeGrammar(file: File): GrammarRepr =
    using(new ObjectInputStream(file.toScala.newInputStream)) {
      _.readObject().asInstanceOf[GrammarRepr]
    }
}
