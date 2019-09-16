package saarland.cispa.se.tribble
package input

import java.io.{File => JFile}

import better.files._
import saarland.cispa.se.tribble.dsl.Grammar

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox


private[tribble] class GrammarLoader(modelAssembler: ModelAssembler, automatonCache: AutomatonCache) {

  private def compile(file: File): Grammar = {
    val toolbox = currentMirror.mkToolBox()
    val code = file.lines mkString "\n"
    val preambula = "import saarland.cispa.se.tribble.dsl._;\n"
    val tree = toolbox.parse(preambula + code)
    toolbox.eval(tree).asInstanceOf[Grammar]
  }

  def loadGrammar(grammarFile: JFile): GrammarRepr = {
    val productions = compile(grammarFile.toScala).productions
    modelAssembler.assemble(productions)
  }
}
