package saarland.cispa.se.tribble
package execution

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.backuity.clist.Command
import org.log4s.getLogger

trait Task {
  def execute(): Unit
}

final class GenerateTask extends Command("generate", "Generate sample inputs")
  with Task with ForestGeneratorModule with OutputModule with RandomnessModule with GrammarModule with CacheModule with RegexModule with ReportingModule with HeuristicModule with CloseOffControlModule {
  private[this] val logger = getLogger

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
