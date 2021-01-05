package de.cispa.se.tribble

import java.io.File
import java.nio.file.{Files, Path}

import de.cispa.se.tribble.generation._
import de.cispa.se.tribble.input._
import org.backuity.clist.{Command, arg, opt}
import org.log4s.getLogger

import scala.util.Random


trait RandomnessModule { self: Command =>
  private val defaultSeed = new Random().nextLong()
  var randomSeed: Long = opt[Long](default = defaultSeed)
  implicit lazy val random: Random = new Random(randomSeed)
}

trait CacheModule { self: Command =>
  private[this] val logger = getLogger
  var automatonDir: File = opt[File](default = new File("automata"), description = "The automaton cache directory. Default \"automata\"")
  lazy val automatonCache: AutomatonCache = {
    Files.createDirectories(automatonDir.toPath)
    new AutomatonCache(automatonDir)
  }
  var ignoreGrammarCache: Boolean = opt[Boolean](default = false, description = "Ignore the grammar cache.")
  var grammarCacheDir: File = opt[File](default = new File("grammar-cache"), description = "The grammar cache directory. Default \"grammar-cache\"")
  lazy val grammarCache: GrammarCache = {
    if (ignoreGrammarCache) {
      logger.info("Not using grammar cache")
      EmptyGrammarCache
    } else {
      logger.info(s"Using grammar cache in $grammarCacheDir")
      Files.createDirectories(grammarCacheDir.toPath)
      new ObjectStreamGrammarCache(grammarCacheDir)
    }
  }
}

trait RegexModule { self: Command =>
  def random: Random
  var minLength: Int = opt[Int](default = 1, description = "The minimum string length to be generated for regexes. Default 1")
  lazy val regexGenerator: RegexGenerator = new RegexGenerator(random, minLength)
}

trait GrammarModule { self: Command =>
  def grammarCache: GrammarCache
  def automatonCache: AutomatonCache
  var damping: Double = opt[Double](default = Double.MinPositiveValue, description = "The damping factor for probabilities. Default Double.MinPositiveValue (4.9e-324)")
  var similarity: Double = opt[Double](default = 1.0d, description = "The similarity factor for probabilities. Default 1.0")
  var unfoldRegexes: Boolean = opt[Boolean](description = "Unfold regular expression literals into productions. (Character ranges are preserved as single derivations)")
  var mergeLiterals: Boolean = opt[Boolean](description = "Merge concatenations of literals into single literals.")
  var checkDuplicateAlternatives: Boolean = opt[Boolean](default = true, name ="no-check-duplicate-alts", description = "Allow duplicate alternatives in alternations.")
  lazy val modelAssembler: ModelAssembler = new ModelAssembler(automatonCache, damping, similarity, unfoldRegexes, mergeLiterals, checkDuplicateAlternatives)
  var compileGrammar: Boolean = opt[Boolean](description = "Compile the grammar with the scala compiler instead of parsing it as data. Not recommended.")
  lazy val grammarLoader: GrammarLoader = new GrammarLoader(modelAssembler, grammarCache)
  var grammarFile: File = arg[File](description = "Path to the grammar file")
  lazy val grammarBuilder: GrammarBuilder = new GrammarBuilder(modelAssembler, grammarCache)
  lazy val grammar: GrammarRepr = if (grammarFile.getName.endsWith(".scala") && compileGrammar) grammarLoader.loadGrammar(grammarFile) else grammarBuilder.buildGrammar(grammarFile)
}

trait ConstraintModule { self: Command =>
  var maxRepetitions: Int = opt[Int](default = 10, description = "Maximum number of repetitions of elements (override quantifications). Default 10")
}

trait HeuristicModule { self: Command =>
  def random: Random
  def grammar: GrammarRepr

  private val nWindowPairNonTermPattern = """(\d+)-window-pair-non-terminal-coverage""".r
  private val kPathNonTermPattern = """(\d+)-path-non-terminal-coverage""".r
  private val kPathPattern = """(\d+)-path-coverage""".r

  var heuristic: String = opt[String](default = "random", description = "The name of the heuristic to use for tree generation. Default random")

  lazy val heuristicImpl: Heuristic = heuristic match {
    case "random" => new RandomChoice(random)
    case "non-terminal-coverage" => new NonTerminalCoverage(random, grammar)
    case nWindowPairNonTermPattern(n) => new NWindowPairNonTerminalCoverage(n.toInt, random, grammar)
    case kPathNonTermPattern(k) => new KPathNonTerminalCoverage(k.toInt, random, grammar)
    case kPathPattern(k) => new KPathCoverage(k.toInt, random, grammar)
    case _ => throw new IllegalArgumentException(s"Unknown heuristic $heuristic")
  }
}

trait ForestationGenerationModule { self: Command =>
  implicit def grammar: GrammarRepr
  implicit def random: Random
  def regexGenerator: RegexGenerator
  def heuristicImpl: Heuristic
  def shortestTreeGenerator: ShortestTreeGenerator

  protected var k: Int = opt[Int](default = 4, description = "The k for k-path coverage.")
  protected var p: Int = opt[Int](default = 20, description = "The number of forests to generate for each size from 1 to #targets.")

  private lazy val goalProvider: PowerSetCoverageGoals = new PowerSetCoverageGoals(k, p)
  lazy val forestationGenerator: Iterator[GoalBasedTreeGenerator] = goalProvider.goals.map(new GoalBasedTreeGenerator(shortestTreeGenerator, _, random))
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
  def maxRepetitions: Int

  private val kPathPattern = """(\d+)-path""".r
  private val kPathDepthPattern = """(\d+)-path-(\d+)""".r
  private val fallOverPattern = """(\d+)-path-(\d+)-fall-over-(\d+)""".r
  private val recurrentFallOverPattern = """recurrent-(\d+)-path-(\d+)-close-off-(\d+)""".r
  private val recurrentKPathPattern = """recurrent-(\d+)-path-(\d+)""".r
  private val recurrentTimedKPathPattern = """recurrent-(\d+)-path-(\d+)-minutes""".r
  private val rndPattern = """(\d+)-random-(\d+)""".r
  private val sizedPattern = """(\d+)-(\d+)-random-(\d+)""".r
  private val depthPattern = """(\d+)-depth-random-(\d+)""".r
  private val probPattern = """(\d+)-probabilistic-(\d+)""".r
  private val limitedProbPattern = """(\d+)-(\d+)-probabilistic-(\d+)""".r
  protected var mode: String = opt[String](default = "mode-not-provided", description =
    """The Construction / Generation mode. Possible values are:
      | <k>-path                              (Generate a set of files with full k-path coverage)
      | <k>-path-<d>                          (Generate a set of files with full k-path coverage, while closing off up to a depth of d)
      | <k>-path-<d>-fall-over-<n>            (Generate n files while going for full k-path coverage first and continuing with max depth d random)
      | recurrent-<k>-path-<n>                (Generate n files with while recurrently targeting k-path coverage goals)
      | recurrent-<k>-path-<d>-close-off-<n>  (Generate n files while recurrently going for k-path coverage but closing off trees with up to d deep random mode)
      | recurrent-<k>-path-<m>-minutes        (Generate files with while recurrently targeting k-path coverage goals. Stop after m minutes)
      | <s>-random-<n>                        (Generate n random files of approximate tree size s)
      | <min>-<max>-random-<n>                (Generate n random files of tree sizes between min and max)
      | <max>-depth-random-<n>                (Generate n random files of depth up to max)
      | <d>-probabilistic-<n>                 (Generate n files adhering to probability annotations ignoring optional elements after depth d)
      | <d>-<c>-probabilistic-<n>             (Generate n files adhering to probability annotations ignoring optional elements after depth d and switching over to shortest derivation after depth c)""".stripMargin)
  lazy val forestGenerator: ForestGenerator = mode match {
    case sizedPattern(min, max, n) => new SizedForestAdapter(min.toInt, max.toInt, n.toInt, heuristicImpl, maxRepetitions)(grammar, random, shortestTreeGenerator)
    case depthPattern(depth, num) => new ForestAdapter(new MaxDepthGenerator(maxRepetitions, random, regexGenerator, depth.toInt, heuristicImpl), num.toInt)
    case fallOverPattern(k, depth, num) => new ContinuingForestAdapter(new GoalBasedTreeGenerator(shortestTreeGenerator, new KPathCoverageGoal(k.toInt), random), new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, depth.toInt, heuristicImpl), num.toInt)
    case recurrentFallOverPattern(k, depth, num) => new ForestSizeLimiter(new GoalBasedTreeGenerator(new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, depth.toInt, heuristicImpl), new RecurrentKPathCoverageGoal(k.toInt), random), num.toInt)
    case rndPattern(avg, num) => new ForestAdapter(new SizedTreeGenerator(maxRepetitions, random, shortestTreeGenerator, avg.toInt, heuristicImpl), num.toInt)
    case kPathPattern(k) => new GoalBasedTreeGenerator(shortestTreeGenerator, new KPathCoverageGoal(k.toInt), random)
    case kPathDepthPattern(k, d) => new GoalBasedTreeGenerator(new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, d.toInt, heuristicImpl), new KPathCoverageGoal(k.toInt), random)
    case recurrentKPathPattern(k, n) => new ForestSizeLimiter(new GoalBasedTreeGenerator(shortestTreeGenerator, new RecurrentKPathCoverageGoal(k.toInt), random), n.toInt)
    case recurrentTimedKPathPattern(k, minutes) => new ForestTimeLimiter(new GoalBasedTreeGenerator(shortestTreeGenerator, new RecurrentKPathCoverageGoal(k.toInt), random), minutes.toInt)
    case probPattern(d, n) => new ForestAdapter(new NaiveProbabilisticTreeGenerator(maxRepetitions, regexGenerator, d.toInt, random, shortestTreeGenerator), n.toInt)
    case limitedProbPattern(d, c, n) => new ForestAdapter(new NaiveProbabilisticTreeGenerator(maxRepetitions, regexGenerator, d.toInt, random, shortestTreeGenerator, c.toInt), n.toInt)
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
