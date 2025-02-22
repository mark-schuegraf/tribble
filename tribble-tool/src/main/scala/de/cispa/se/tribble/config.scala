package de.cispa.se.tribble

import de.cispa.se.tribble.generation._
import de.cispa.se.tribble.input._
import de.cispa.se.tribble.output._
import de.cispa.se.tribble.transformation._
import de.cispa.se.tribble.transformation.grammar_adaptation._
import de.cispa.se.tribble.transformation.normal_forms._
import org.backuity.clist.{Command, arg, opt}
import org.log4s.getLogger

import java.io.File
import java.nio.file.{Files, Path}
import scala.util.Random


trait RandomnessModule { self: Command =>
  private val defaultSeed = new Random().nextLong()
  var randomSeed: Long = opt[Long](default = defaultSeed)
  implicit lazy val random: Random = new Random(randomSeed)
}

trait CacheModule { self: Command =>
  private val logger = getLogger
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
  var checkIds: Boolean = opt[Boolean](default = true, name ="no-check-ids", description = "Allow inconsistent ids in derivation rules.")
  var assignProbabilities: Boolean = opt[Boolean](default = true, name ="no-assign-prob", description = "Do not assign probabilities to derivation rules.")
  var epsilonizeQuantifications: Boolean = opt[Boolean](description = "Turn optional parts of quantifications into alternations.")
  lazy val modelAssembler: ModelAssembler = new ModelAssembler(automatonCache, damping, similarity, unfoldRegexes, mergeLiterals, checkDuplicateAlternatives, checkIds, assignProbabilities, epsilonizeQuantifications)

  var loadingStrategy: String = opt[String](default = "parse", description = "How to process the grammar file. Valid options are parse, compile, and unmarshal.")
  lazy val loadingStrategyImpl: LoadingStrategy = loadingStrategy match {
    case "parse" => ParseGrammar(modelAssembler)
    case "compile" => CompileGrammar(modelAssembler)
    case "unmarshal" => UnmarshalGrammar
    case _ => throw new IllegalArgumentException(s"Unknown loading strategy $loadingStrategy")
  }
  lazy val grammarLoader: GrammarLoader = new GrammarLoader(loadingStrategyImpl, grammarCache)
  var grammarFile: File = arg[File](description = "Path to the grammar file")
  lazy val grammar: GrammarRepr = grammarLoader.loadGrammar(grammarFile)
  lazy val reachability: Reachability = new Reachability(grammar)
}

trait ConstraintModule { self: Command =>
  var maxRepetitions: Int = opt[Int](default = 10, description = "Maximum number of repetitions of elements (override quantifications). Default 10")
}

trait HeuristicModule { self: Command =>
  def random: Random
  def reachability: Reachability
  def grammar: GrammarRepr

  private val nWindowPairNonTermPattern = """(\d+)-window-pair-non-terminal-coverage""".r
  private val kPathNonTermPattern = """(\d+)-path-non-terminal-coverage""".r
  private val kPathPattern = """(\d+)-path-coverage""".r

  var heuristic: String = opt[String](default = "random", description = "The name of the heuristic to use for tree generation. Default random")

  lazy val heuristicImpl: Heuristic = heuristic match {
    case "random" => new RandomChoice(random)
    case "least-recently-used" => new LRUChoice(random)
    case "non-terminal-coverage" => new NonTerminalCoverage(random, grammar)
    case nWindowPairNonTermPattern(n) => new NWindowPairNonTerminalCoverage(n.toInt, random, grammar)
    case kPathNonTermPattern(k) => new KPathNonTerminalCoverage(k.toInt, random, grammar)
    case kPathPattern(k) => new KPathCoverage(k.toInt, random, grammar, reachability)
    case _ => throw new IllegalArgumentException(s"Unknown heuristic $heuristic")
  }
}

trait ForestationGenerationModule { self: Command =>
  implicit def grammar: GrammarRepr
  implicit def reachability: Reachability
  implicit def random: Random
  def regexGenerator: RegexGenerator
  def heuristicImpl: Heuristic
  def shortestTreeGenerator: ShortestTreeGenerator

  protected var k: Int = opt[Int](default = 4, description = "The k for k-path coverage.")
  protected var p: Int = opt[Int](default = 20, description = "The number of forests to generate for each size from 1 to #targets.")

  private lazy val goalProvider: PowerSetCoverageGoals = new PowerSetCoverageGoals(k, p)
  lazy val forestationGenerator: Iterator[GoalBasedTreeGenerator] = goalProvider.goals.map(new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, _))
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
  implicit def reachability: Reachability
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
  private val incrementingKPathPattern = """incrementing-(\d+)-path-(\d+)""".r
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
      | recurrent-<k>-path-<m>-minutes        (Generate files while recurrently targeting k-path coverage goals. Stop after m minutes)
      | incrementing-<k>-path-<m>-minutes     (Generate files while targeting k-path coverage goals for ever increasing k. Stop after m minutes)
      | <s>-random-<n>                        (Generate n random files of approximate tree size s)
      | <min>-<max>-random-<n>                (Generate n random files of tree sizes between min and max)
      | <max>-depth-random-<n>                (Generate n random files of depth up to max, or else as deep as the shortest derivation)
      | <d>-probabilistic-<n>                 (Generate n files adhering to probability annotations ignoring optional elements after depth d)
      | <d>-<c>-probabilistic-<n>             (Generate n files adhering to probability annotations ignoring optional elements after depth d and switching over to shortest derivation after depth c)""".stripMargin)
  lazy val forestGenerator: ForestGenerator = mode match {
    case sizedPattern(min, max, n) => new SizedForestAdapter(min.toInt, max.toInt, n.toInt, heuristicImpl, maxRepetitions)(grammar, random, shortestTreeGenerator)
    case depthPattern(depth, num) => new ForestAdapter(new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, depth.toInt, heuristicImpl), num.toInt)
    case fallOverPattern(k, depth, num) => new ContinuingForestAdapter(new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, new KPathCoverageGoal(k.toInt)), new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, depth.toInt, heuristicImpl), num.toInt)
    case recurrentFallOverPattern(k, depth, num) => new ForestSizeLimiter(new GoalBasedTreeGenerator(new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, depth.toInt, heuristicImpl), random)(grammar, new RecurrentKPathCoverageGoal(k.toInt)), num.toInt)
    case rndPattern(avg, num) => new ForestAdapter(new SizedTreeGenerator(maxRepetitions, random, shortestTreeGenerator, avg.toInt, heuristicImpl), num.toInt)
    case kPathPattern(k) => new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, new KPathCoverageGoal(k.toInt))
    case kPathDepthPattern(k, d) => new GoalBasedTreeGenerator(new BestEffortMaxDepthGenerator(maxRepetitions, random, regexGenerator, d.toInt, heuristicImpl), random)(grammar, new KPathCoverageGoal(k.toInt))
    case recurrentKPathPattern(k, n) => new ForestSizeLimiter(new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, new RecurrentKPathCoverageGoal(k.toInt, n.toInt)), n.toInt)
    case recurrentTimedKPathPattern(k, minutes) => new ForestTimeLimiter(new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, new RecurrentKPathCoverageGoal(k.toInt)), minutes.toInt)
    case incrementingKPathPattern(k, minutes) => new ForestTimeLimiter(new GoalBasedTreeGenerator(shortestTreeGenerator, random)(grammar, new IncrementingKPathCoverageGoal(k.toInt)), minutes.toInt)
    case probPattern(d, n) => new ForestAdapter(new NaiveProbabilisticTreeGenerator(maxRepetitions, regexGenerator, d.toInt, random, shortestTreeGenerator), n.toInt)
    case limitedProbPattern(d, c, n) => new ForestAdapter(new NaiveProbabilisticTreeGenerator(maxRepetitions, regexGenerator, d.toInt, random, shortestTreeGenerator, c.toInt), n.toInt)
    case _ => throw new IllegalArgumentException(s"Unknown mode '$mode'")
  }
}

trait GrammarTransformationModule { self: Command =>
  /* Normal form patterns */
  private val backusNaurFormalizerPattern = """backus-naur-formalizer"""
  private val extendedChomskyFormalizerPattern = """extended-chomsky-normal-formalizer"""
  private val chomskyFormalizerPattern = """chomsky-normal-formalizer"""
  private val extendedGreibachFormalizerPattern = """extended-greibach-normal-formalizer"""
  private val greibachFormalizerPattern = """greibach-normal-formalizer"""
  /* Normal form substep patterns */
  private val nonsolitaryTerminalExtractionPattern = """nonsolitary-terminal-extraction"""
  private val nonbinaryRuleReductionPattern = """nonbinary-rule-reduction"""
  private val deletionRuleEliminationPattern = """deletion-rule-elimination"""
  private val unitRuleEliminationPattern = """unit-rule-elimination"""
  private val leftRecursionLinearizationPattern = """left-recursion-linearization"""
  /* Grammar adaptation framework patterns */
  private val alternationExtractionPattern = """internal-alternation-extraction"""
  private val concatenationExtractionPattern = """internal-concatenation-extraction"""
  private val ruleInliningPattern = """(\d+)-level-rule-inlining""".r
  private val nonrecursiveRuleInliningPattern = """(\d+)-level-nonrecursive-rule-inlining""".r
  private val quantificationEliminationPattern = """quantification-elimination"""
  private val quantificationExpansionPattern = """(\d+)-fold-quantification-expansion""".r
  /* Utility transformation patterns */
  private val identityPattern = """identity"""
  private val duplicateAlternativeEliminationPattern = """duplicate-alternative-elimination"""
  /* Command line documentation for the various transformers */
  protected var mode: String = opt[String](default = "mode-not-provided", description =
    """The Grammar Transformation mode. Possible values are:
      | -- Normal Forms --
      | backus-naur-formalizer                 (Puts the grammar into Backus-Naur form)
      | extended-chomsky-normal-formalizer     (Requires BNF: Puts the grammar into extended Chomsky normal form)
      | chomsky-normal-formalizer              (Requires BNF: Puts the grammar into Chomsky normal form)
      | extended-greibach-normal-formalizer    (Requires ECNF: Puts the grammar into extended Greibach normal form)
      | greibach-normal-formalizer             (Requires CNF: Puts the grammar into Greibach normal form)
      | -- Normal Form Substeps --
      | nonsolitary-terminal-extraction        (Requires BNF: Extract all non-solitary terminals into their own rules)
      | nonbinary-rule-reduction               (Requires BNF: Splits concatenations with more than two elements into a rule cascade)
      | deletion-rule-elimination              (Requires BNF: Eliminates deletion rules in non-start productions)
      | unit-rule-elimination                  (Requires BNF: Eliminates unit rules in non-start productions)
      | left-recursion-linearization           (Requires ECNF: Linearizes referencing order of leftmost references.)
      | -- Grammar Adaptation Framework --
      | internal-alternation-extraction        (Extracts all internal alternations into top level)
      | internal-concatenation-extraction      (Extracts all internal concatenation into top level)
      | <k>-level-rule-inlining                (Inlines rules into their reference sites, up to k times in the recursive case)
      | <k>-level-nonrecursive-rule-inlining   (Inlines rules into only their acyclic reference sites, up to k times in the recursive case)
      | <k>-fold-quantification-expansion      (Partially folds at most k alternatives of each quantification into a top-level iterative rule)
      | quantification-elimination             (Folds each quantification into a top-level right-recursive rule)
      | -- Utility Transformations --
      | identity                               (Returns the input grammar)
      | duplicate-alternative-elimination      (Prunes duplicate alternatives)""".stripMargin)
  /* Keep in mind that the random seed from the RandomnessModule can be passed to constructors of random transformations here*/
  lazy val grammarTransformer: GrammarTransformer = mode match {
    /* Normal forms */
    case `backusNaurFormalizerPattern` => BackusNaurFormalizer
    case `extendedChomskyFormalizerPattern` => ExtendedChomskyFormalizer
    case `chomskyFormalizerPattern` => ChomskyFormalizer
    case `extendedGreibachFormalizerPattern` => ExtendedGreibachFormalizer
    case `greibachFormalizerPattern` => GreibachFormalizer
    /* Normal form substeps */
    case `nonsolitaryTerminalExtractionPattern` => NonsolitaryTerminalExtraction
    case `nonbinaryRuleReductionPattern` => NonbinaryRuleReduction
    case `deletionRuleEliminationPattern` => DeletionRuleElimination
    case `unitRuleEliminationPattern` => UnitRuleElimination
    case `leftRecursionLinearizationPattern` => LeftRecursionLinearization
    /* Grammar adaptation framework */
    case `alternationExtractionPattern` => AlternationExtraction
    case `concatenationExtractionPattern` => ConcatenationExtraction
    case ruleInliningPattern(inlineLevels) => new RuleInlining(inlineLevels.toInt)
    case nonrecursiveRuleInliningPattern(inlineLevels) => new NonrecursiveRuleInlining(inlineLevels.toInt)
    case `quantificationEliminationPattern` => QuantificationElimination
    case quantificationExpansionPattern(repetitionLimit) => new QuantificationExpansion(repetitionLimit.toInt)
    /* Utility transformations */
    case `identityPattern` => IdentityTransformer
    case `duplicateAlternativeEliminationPattern` => DuplicateAlternativeElimination
    case _ => throw new IllegalArgumentException(s"Unknown mode '$mode'")
  }
}

trait GrammarOutputModule { self: Command =>
  var outputGrammarFile: File = arg[File](description = "Path to the output grammar file.")
  var storingStrategy: String = opt[String](default = "print", description = "How to persist the grammar. Valid options are 'print' and 'marshal'.")
  lazy val grammarStorer: StoringStrategy = storingStrategy match {
    case "print" => PrintGrammar
    case "marshal" => MarshalGrammar
    case _ => throw new IllegalArgumentException(s"Unknown storing strategy $storingStrategy")
  }
}

trait OutputModule { self: Command =>
  var suffix: String = opt[String](description = "The file ending. Default .txt", default = ".txt")
  var outDir: File = opt[File](description = "Where to put the generated files. Default \"out\"", default = new File("out"))
  lazy val outputDir: Path = Files.createDirectories(outDir.toPath)
}

trait ReportingModule { self: Command =>
  implicit def grammar: GrammarRepr
  implicit def reachability: Reachability
  implicit def random: Random
  var reportKCoverage: Int = opt[Int](description = "Up to which k to report the k-path coverage. Only effective when report-file is set. Default 4", default = 4)
  var reportFile: Option[File] = opt[Option[File]](description = "Where to log the k-path coverage")
  lazy val reporter: TreeReporter = reportFile.map(new KPathReporter(_, reportKCoverage)).getOrElse(NopReporter)
}
