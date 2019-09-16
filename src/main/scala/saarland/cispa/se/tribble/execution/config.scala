package saarland.cispa.se.tribble
package execution

import java.io.File
import java.nio.file.{Files, Path}

import org.backuity.clist.{Command, arg, opt}
import saarland.cispa.se.tribble.generation._
import saarland.cispa.se.tribble.input.{AutomatonCache, GrammarBuilder, GrammarLoader, ModelAssembler}

import scala.util.Random


trait RandomnessModule { self: Command =>
  private val defaultSeed = new Random().nextLong()
  var randomSeed: Long = opt[Long](default = defaultSeed)
  implicit lazy val random: Random = new Random(randomSeed)
}

trait CacheModule { self: Command =>
  var automatonDir: File = opt[File](default = new File("automata"), description = "The automaton cache directory. Default \"automata\"")
  lazy val automatonCache: AutomatonCache = {
    Files.createDirectories(automatonDir.toPath)
    new AutomatonCache(automatonDir)
  }
}

trait RegexModule { self: Command =>
  def random: Random
  var minLength: Int = opt[Int](default = 1, description = "The minimum string length to be generated for regexes. Default 1")
  lazy val regexGenerator: RegexGenerator = new RegexGenerator(random, minLength)
}

trait GrammarModule { self: Command =>
  def automatonCache: AutomatonCache
  var maxRepetitions: Int = opt[Int](default = 10, description = "Maximum number of repetitions of elements (override quantifications). Default 10")
  var damping: Double = opt[Double](default = Double.MinPositiveValue, description = "The damping factor for probabilities. Default Double.MinPositiveValue (4.9e-324)")
  var similarity: Double = opt[Double](default = 1.0d, description = "The similarity factor for probabilities. Default 1.0")
  var unfoldRegexes: Boolean = opt[Boolean](description = "Unfold regular expression literals into productions. (Character ranges are preserved as single derivations)")
  var mergeLiterals: Boolean = opt[Boolean](description = "Merge concatenations of literals into single literals.")
  lazy val modelAssembler: ModelAssembler = new ModelAssembler(maxRepetitions, automatonCache, damping, similarity, unfoldRegexes, mergeLiterals)
  lazy val grammarLoader: GrammarLoader = new GrammarLoader(modelAssembler, automatonCache)
  var grammarFile: File = arg[File](description = "Path to the grammar file")
  lazy val grammarBuilder: GrammarBuilder = new GrammarBuilder(modelAssembler)
  lazy val grammar: GrammarRepr = if (grammarFile.getName.endsWith(".scala")) grammarLoader.loadGrammar(grammarFile) else grammarBuilder.buildGrammar(grammarFile)
}

trait HeuristicModule { self: Command =>
  def random: Random
  def grammar: GrammarRepr

  private val kPathPattern = """(\d+)-path-coverage""".r

  var heuristic: String = opt[String](default = "random", description = "The name of the heuristic to use for tree generation. Default random")

  lazy val heuristicImpl: Heuristic = heuristic match {
    case "random" => new RandomChoice(random)
    case kPathPattern(k) => new KPathCoverage(k.toInt, random, grammar)
    case _ => throw new IllegalArgumentException(s"Unknown heuristic $heuristic")
  }
}

trait CloseOffControlModule {self: Command =>
  def random: Random
  def regexGenerator: RegexGenerator

  var closeOffChoice: Int = opt[Int](default = 1, description =
    """When closing-off trees, follow the NUMth shortest path.
      |Valid values start at 1.
      |Converges to 1 for recurring derivations.""".stripMargin)
  lazy val shortestTreeGenerator: ShortestTreeGenerator = new ShortestTreeGenerator(regexGenerator, random, closeOffChoice)
}

trait ForestGeneratorModule { self: Command =>
  implicit def grammar: GrammarRepr
  implicit def random: Random
  def regexGenerator: RegexGenerator
  def heuristicImpl: Heuristic
  def shortestTreeGenerator: ShortestTreeGenerator

  private val kPathPattern = """(\d+)-path""".r
  private val kPathDepthPattern = """(\d+)-path-(\d+)""".r
  protected var mode: String = opt[String](default = "mode-not-provided", description =
    """The Construction / Generation mode. Possible values are:
      | <k>-path        (Generate a set of files with full k-path coverage)
      | <k>-path-<d>    (Generate a set of files with full k-path coverage, while closing off up to a depth of d)""".stripMargin)
  lazy val forestGenerator: ForestGenerator = mode match {
    case kPathPattern(k) => new GoalBasedTreeGenerator(shortestTreeGenerator, new KPathCoverageGoal(k.toInt))
    case kPathDepthPattern(k, d) => new GoalBasedTreeGenerator(new BestEffortMaxDepthGenerator(random, regexGenerator, d.toInt, heuristicImpl), new KPathCoverageGoal(k.toInt))
    case _ => throw new IllegalArgumentException(s"Unknown mode '$mode'")
  }
}

trait OutputModule { self: Command =>
  var suffix: String = opt[String](description = "The file ending. Default .txt", default = ".txt")
  var outDir: File = opt[File](description = "Where to put the generated files. Default \"out\"", default = new File("out"))
  lazy val outputDir: Path = Files.createDirectories(outDir.toPath)
}

trait ReportingModule { self: Command =>
  implicit def grammar: GrammarRepr
  implicit def random: Random
  var reportKCoverage: Int = opt[Int](description = "Up to which k to report the k-path coverage. Only effective when report-file is set. Default 4", default = 4)
  var reportFile: Option[File] = opt[Option[File]](description = "Where to log the k-path coverage")
  lazy val reporter: TreeReporter = reportFile.map(new KPathReporter(_, reportKCoverage)).getOrElse(NopReporter)
}
