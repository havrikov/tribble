package de.cispa.se.tribble
package input

import java.io.{File => JFile}

import better.files._
import de.cispa.se.tribble.dsl.Grammar

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox


private[tribble] class GrammarLoader(modelAssembler: ModelAssembler, grammarCache: GrammarCache) {

  private def compile(file: File): Grammar = {
    val toolbox = currentMirror.mkToolBox()
    val code = file.lines mkString "\n"
    val preambula = "import de.cispa.se.tribble.dsl._;\n"
    val tree = toolbox.parse(preambula + code)
    toolbox.eval(tree).asInstanceOf[Grammar]
  }

  def loadGrammar(grammarFile: JFile): GrammarRepr = {
    grammarCache.loadGrammar(grammarFile.computeHash()) match {
      case Some(grammar) => grammar
      case None =>
        val productions = compile(grammarFile.toScala).productions
        modelAssembler.assemble(productions)
    }
  }
}
