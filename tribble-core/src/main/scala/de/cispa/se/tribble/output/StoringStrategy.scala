package de.cispa.se.tribble
package output

import de.cispa.se.tribble.input._
import org.log4s.getLogger

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

sealed trait StoringStrategy {
  def store(file: File, grammar: GrammarRepr): Unit
}

/** Stores the grammar in text form. The default choice. */
case object PrintGrammar extends StoringStrategy {
  private val logger = getLogger

  override def store(file: File, grammar: GrammarRepr): Unit = {
    logger.info(s"Attempting to print grammar to $file")
    if (!file.getName.endsWith(".scala"))
      logger.warn(s"The path to the output grammar file does not have a '.scala' suffix: $file. " +
        "Expected suffix '.scala' for the 'print' storing strategy. Other suffixes are purely symbolic.")
    GrammarStatistics.process(grammar)
    logger.trace(s"Output grammar: $grammar")
    FileValidityChecker.checkAndMakeDirs(file)
    val serialized: String = new GrammarPrettyPrinter(grammar).prettyPrint()
    Files.write(file.toPath, serialized.getBytes(StandardCharsets.UTF_8))
    logger.info(s"Printed grammar to $file. Grammar: $grammar")
  }

}

/** Directly serializes a binary grammar object. */
case object MarshalGrammar extends StoringStrategy {
  private val logger = getLogger

  override def store(file: File, grammar: GrammarRepr): Unit = {
    logger.info(s"Attempting to marshal grammar to $file")
    GrammarStatistics.process(grammar)
    logger.trace(s"Output grammar: $grammar")
    /* Transformation changes grammar structure, so the shortest derivations need to be recomputed.
    To do so, shortest derivations need to be reset as they may contain outdated values instead of the default sentinel.  */
    val derivationsReset = ShortestDerivationReset.process(grammar)
    val derivationsRecomputed = ShortestDerivationComputation.process(derivationsReset)
    FileValidityChecker.checkAndMakeDirs(file)
    GrammarSerializer.serializeGrammar(derivationsRecomputed, file)
    logger.info(s"Marshaled grammar to $file")
  }

}

private[tribble] object FileValidityChecker {

  def checkAndMakeDirs(file: File): Unit = {
    Files.createDirectories(file.toPath.getParent)
    try {
      Files.createFile(file.toPath)
    } catch {
      case e: IOException => throw new IllegalArgumentException(s"Refusing to overwrite existing grammar file at: '$file'", e)
    }
  }

}
