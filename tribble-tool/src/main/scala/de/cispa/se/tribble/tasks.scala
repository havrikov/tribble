package de.cispa.se.tribble

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import de.cispa.se.tribble.Internal._
import de.cispa.se.tribble.input.{AlternativeExtraction, ObjectStreamGrammarCache, RuleInlining}
import de.cispa.se.tribble.output.GrammarPrettyPrinter
import org.backuity.clist.{Command, opt}
import org.log4s.getLogger

trait Task {
  def execute(): Unit
}

final class GenerateTask extends Command("generate", "Generate sample inputs")
  with Task with ForestGeneratorModule with OutputModule with RandomnessModule with GrammarModule with CacheModule with RegexModule with ReportingModule with HeuristicModule with CloseOffControlModule with ConstraintModule {
  private val logger = getLogger

  override def execute(): Unit = {
    logger.info(s"Using random seed $randomSeed")
    logger.info(s"Writing generated files to $outputDir")

    val trees = forestGenerator.generateForest()

    for ((tree, i) <- trees.zipWithIndex) {
      reporter.processTree(i + 1, tree)
      val input = tree.leaves.mkString
      val path = Files.write(Files.createTempFile(outputDir, f"file${i + 1}%06d_${tree.size()}%d_${tree.depth()}%d_", suffix), input.getBytes(StandardCharsets.UTF_8))
      logger.debug(s"Generated $path")
    }

  }
}

final class InlineGrammarTask extends Command("inline", "Output grammar with inlined productions")
  with Task with CacheModule with OutputModule with GrammarModule {
  var inlineLevels: Int = opt[Int](description = "How many times to perform inlining. Default 1", default = 1)

  override def execute(): Unit = {
    val inlined = new RuleInlining(inlineLevels).process(grammar)
    val serialized = new GrammarPrettyPrinter(inlined).prettyPrint()
    Files.write(Files.createFile(outputDir.resolve(grammarFile.getName)), serialized.getBytes(StandardCharsets.UTF_8))
  }
}

final class ExtractAlternativesTask extends Command("extract-alternatives", "Output grammar with all alternatives extracted to top level")
  with Task with CacheModule with OutputModule with GrammarModule {

  override def execute(): Unit = {
    val extracted = AlternativeExtraction.process(grammar)
    val serialized = new GrammarPrettyPrinter(extracted).prettyPrint()
    Files.write(Files.createFile(outputDir.resolve(grammarFile.getName)), serialized.getBytes(StandardCharsets.UTF_8))
  }
}

final class GenerateForestationTask extends Command(name = "forestation", description = "Generate a forest of forests spanning a k-path coverage saturation range")
  with Task with ForestationGenerationModule with OutputModule with RandomnessModule with GrammarModule with CacheModule with RegexModule with HeuristicModule with CloseOffControlModule {
  private val logger = getLogger

  override def execute(): Unit = {
    logger.info(s"Using random seed $randomSeed")
    logger.info(s"Writing generated forests to $outputDir")

    val forests = forestationGenerator.map(_.generateForest())
    for ((forest, j) <- forests.zipWithIndex) {
      val dir = Files.createDirectory(outputDir.resolve(f"forest$j%06d"))
      logger.debug(s"Creating forest $j")
      val reporter = new KPathReporter(Files.createFile(outputDir.resolve(f"forest$j%06d.csv")).toFile, k)(grammar, random, reachability)
      for ((tree, i) <- forest.zipWithIndex) {
        reporter.processTree(i + 1, tree)
        val input = tree.leaves.mkString
        Files.write(Files.createTempFile(dir, f"file${i + 1}%06d_${tree.size()}%d_", suffix), input.getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}

final class CacheGrammarTask extends Command("cache-grammar", "Put a grammar into the cache for faster loading in the future")
  with Task with GrammarModule with CacheModule {
  override def execute(): Unit = {
    // ignore the grammarCache from the CacheModule because it might be the EmptyGrammarCache
    Files.createDirectories(grammarCacheDir.toPath)
    val cache = new ObjectStreamGrammarCache(grammarCacheDir)
    cache.storeGrammar(grammar, grammarHash(grammarFile))
  }
}
