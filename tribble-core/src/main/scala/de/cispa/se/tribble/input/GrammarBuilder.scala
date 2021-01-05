package de.cispa.se.tribble
package input

import java.io.{File => JFile}
import java.nio.charset.StandardCharsets

import better.files._
import fastparse._

class GrammarBuilder(modelAssembler: ModelAssembler, grammarCache: GrammarCache) {

  def buildGrammar(grammarFile: JFile): GrammarRepr = {
    grammarCache.loadGrammar(grammarFile.computeHash()) match {
      case Some(grammar) => grammar
      case None =>
        val file = grammarFile.toScala
        val parser = if (file.extension.contains(".scala")) ScalaDSLParser else TextDSLParser
        parse(ParserInput.fromString(file.lines(StandardCharsets.UTF_8) mkString "\n"), parser.grammar(_)).fold(
          (label, index, extra) => throw new IllegalArgumentException(s"Error while parsing $label at position ${extra.input.prettyIndex(index)}"),
          (rules, _) => modelAssembler.assemble(rules)
        )
    }
  }

}
