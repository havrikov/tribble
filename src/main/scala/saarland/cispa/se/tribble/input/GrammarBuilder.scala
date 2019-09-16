package saarland.cispa.se.tribble
package input

import java.io.{File => JFile}
import java.nio.charset.StandardCharsets

import better.files._
import fastparse._

class GrammarBuilder(modelAssembler: ModelAssembler) {

  def buildGrammar(grammarFile: JFile): GrammarRepr = {
    val file = grammarFile.toScala
    parse(ParserInput.fromIterator(file.lineIterator(StandardCharsets.UTF_8).map(_ + '\n')), GrammarParser.grammar(_)).fold(
      (label, index, _) => throw new IllegalArgumentException(s"Error while parsing $label at index $index"),
      (rules, _) => modelAssembler.assemble(rules)
    )
  }

}
