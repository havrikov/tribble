package de.cispa.se.tribble
package output

import org.apache.commons.text.{StringEscapeUtils, WordUtils}

import scala.collection.mutable

object DTreeUtils {

  /** Adds the toDot: String function to DTrees. */
  implicit class RichDTree(val tree: DTree) extends AnyVal {
    def toDot: String = {
      val b = new mutable.StringBuilder("graph DerivationTree {\nrankdir = TB;\nfillcolor = grey;\n")
      tree._toDot(b)
      b.append("}\n")
      b.mkString
    }

    private[tribble] def _toDot(b: StringBuilder): Unit = {
      tree match {
        case DLeaf(_, _, value) =>
          b.append(s"""{rank = sink; ${System.identityHashCode(this)} [shape=box,label="""")
          escapeDot(value, b)
          b.append("\"];}\n")
        case DNode(decl, _, children) =>
          val style = decl match {
            case _: Reference | _: TerminalRule => "style=filled,"
            case _ => ""
          }
          b.append(s"""${System.identityHashCode(this)} [shape=egg,${style}label="""")
          val label = new mutable.StringBuilder()
          escapeDot(decl.toString, label)
          b.append(WordUtils.wrap(label.mkString, 60))
          b.append("\"];\n")
          children.values.foreach(c => b.append(s"${System.identityHashCode(this)} -- ${System.identityHashCode(c)};\n"))
          children.values.foreach(_._toDot(b))
      }
    }

    private def escapeDot(s: String, b: StringBuilder): Unit = {
      b.append(StringEscapeUtils.escapeJson(s))
    }

  }

  // For Java compatibility
  def toDot(tree: DTree): String = tree.toDot
}
