package de.cispa.se

import java.io.{File => JFile}
import java.math.BigInteger
import java.security.{DigestInputStream, MessageDigest}

import better.files._

package object tribble {
  private[tribble] type NonTerminal = String
  private[tribble] type Production = (NonTerminal, DerivationRule)

  implicit class RichFile(val file: JFile) extends AnyVal {
    /** Produces a string representation of the sha256 hash of this file's contents. */
    def computeHash(): String = {
      val digest = MessageDigest.getInstance("SHA-256")
      val grammarFile = file.toScala
      for {
        fi <- grammarFile.newInputStream.autoClosed
        di <- new DigestInputStream(fi, digest).autoClosed
      }
        di.readAllBytes()
      val bytes = digest.digest()
      new BigInteger(1, bytes).toString(16)
    }
  }
}
