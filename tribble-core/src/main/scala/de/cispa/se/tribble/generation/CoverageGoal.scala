package de.cispa.se.tribble
package generation

import scala.collection.mutable
import scala.util.Random

/** A Goal consists of multiple Targets */
trait CoverageGoal {
  def name: String
  def usedDerivation(rule: DerivationRule, parent: Option[DNode]): Unit
  def cost(from: DerivationRule): Int
  def targetReached: Boolean
  def nextTarget(): Boolean
  def targetCount: Int
  def coveredTargets: Int
}


class KPathCoverageGoal(k: Int)(implicit grammar: GrammarRepr, random: Random, reach: Reachability) extends CoverageGoal {
  require(k > 0, s"k must be greater than one! ($k given)")
  val name = s"$k-path coverage"
  protected[tribble] val targets: mutable.Set[List[DerivationRule]] = {
    // step 1/2: gather the targets in a deterministic order
    val linearTargets = if (k == 1) {
      reach.interestingRules.map(List(_))
    } else {
      reach.interestingRules.collect { case ref: Reference => List(ref) }.flatMap(calcTuples)
    }
    // step 2/2: shuffle the targets
    mutable.Set(random.shuffle(linearTargets).toSeq:_*)
  }
  // the target is ordered such that the next rule to be reached is in head position. E.g. root :: child1 :: grandchild3 :: Nil
  protected var target: List[DerivationRule] = Nil

  override val targetCount: Int = targets.size

  override def coveredTargets: Int = targetCount - targets.size

  override def nextTarget(): Boolean = {
    require(targetReached, s"Cannot select a new target because the current target has not been reached! $target")
    val hasNext = targets.nonEmpty
    if (hasNext) target = targets.head
    hasNext
  }

  override def usedDerivation(rule: DerivationRule, parent: Option[DNode]): Unit = {
    val tuple = getTupleEndingIn(rule, parent)
    if (tuple.nonEmpty)
      targets -= tuple
    if (target.headOption.contains(rule))
      target = target.tail
  }

  /** Weight of the shortest path from [[from]] to [[target]] or Int.MaxValue if not reachable. */
  override def cost(from: DerivationRule): Int = {
    assert(target.nonEmpty, s"Asking for the cost of deriving $from with no current goal!")
    val t = target.head
    if (from == t) 0 else reach.reachability(from).getOrElse(t, Int.MaxValue)
  }

  override def targetReached: Boolean = target.isEmpty

  /** Collects possible k-paths starting with the given prefix. */
  protected def calcTuples(prefix: List[Reference]): Set[List[DerivationRule]] = {
    require(prefix.nonEmpty)
    require(prefix.size < k)
    val start = prefix.head
    val immediateSteps = reach.immediateSuccessors(start)
    if (prefix.size == k - 1) {
      // last element: nonterminals and terminals allowed as last element
      immediateSteps.collect { case x@(_: Reference | _: TerminalRule) => (x :: prefix).reverse }
    } else {
      // only interested in nonterminals
      immediateSteps.collect { case ref: Reference => ref :: prefix }.flatMap(calcTuples)
    }
  }

  private def lookUp(p: Option[DNode], limit: Int): mutable.ListBuffer[DerivationRule] = {
    val prefix = mutable.ListBuffer[DerivationRule]()
    var n = p
    while (prefix.size < limit && n.isDefined) {
      n.get.decl match {
        case r: Reference => prefix.prepend(r)
        case _ =>
      }
      n = n.get.parent
    }
    prefix
  }

  protected def getTupleEndingIn(rule: DerivationRule, parent: Option[DNode]): List[DerivationRule] = rule match {
    case _: Reference | _: TerminalRule =>
      val prefix = lookUp(parent, k - 1)
      prefix.append(rule)
      prefix.toList
    case _ => Nil
  }

}

class RecurrentKPathCoverageGoal(k: Int, path_limit: Int = Int.MaxValue)(implicit grammar: GrammarRepr, random: Random, reach: Reachability) extends KPathCoverageGoal(k) {
  // counter to ensure roughly path_limit paths are calculated
  private var targetsEnumerated = 0

  // backup of original targets
  private val targetPool = targets.toList

  // never ending targets
  override val targetCount: Int = Int.MaxValue

  override def usedDerivation(rule: DerivationRule, parent: Option[DNode]): Unit = {
    val tuple = getTupleEndingIn(rule, parent)
    targets -= tuple
    // difference from superclass: refill targets
    if (targets.isEmpty)
      targets ++= random.shuffle(targetPool)
    if (target.headOption.contains(rule))
      target = target.tail
  }

/** Collects possible k-paths starting with the given prefix, but stops after approximately [[path_limit]] tuples have been enumerated.
  * In recurrent generation mode not all paths can feasibly be covered, so only some need to be calculated. */
  override def calcTuples(prefix: List[Reference]): Set[List[DerivationRule]] = {
    val tuples = limitedCalcTuples(prefix)
    tuples.toSet
  }

  /** Tuple enumeration order is dependent on the random seed, so subsequent generation calls explore a different part of the k-tuple space. */
  private def limitedCalcTuples(prefix: List[Reference]): List[List[DerivationRule]] = {
    require(prefix.nonEmpty)
    require(prefix.size < k)

    if (targetsEnumerated >= path_limit)
      return List.empty

    val start = prefix.head
    val immediateSteps = reach.immediateSuccessors(start).toList
    val shuffledSteps = random.shuffle(immediateSteps)
    if (prefix.size == k - 1) {
      // last element: nonterminals and terminals allowed as last element
      val res = shuffledSteps.collect { case x@(_: Reference | _: TerminalRule) => (x :: prefix).reverse }
      targetsEnumerated += res.size
      res
    } else {
      // only interested in nonterminals
      shuffledSteps.collect { case ref: Reference => ref :: prefix }.flatMap(calcTuples)
    }
  }
}

class IncrementingKPathCoverageGoal(private var k: Int)(implicit grammar: GrammarRepr, random: Random, reach: Reachability) extends CoverageGoal {
  private val logger = org.log4s.getLogger
  private var currentGoal: KPathCoverageGoal = new KPathCoverageGoal(k)

  logger.info(s"Starting incrementing $k-path")

  val name: String = s"incrementing $k-path coverage"

  override def usedDerivation(rule: DerivationRule, parent: Option[DNode]): Unit = {
    currentGoal.usedDerivation(rule, parent)
    // if the current goal has been completely reached
    if (targetReached && currentGoal.targets.isEmpty) {
      logger.info(s"Exhausted $k-path coverage")
      k += 1
      currentGoal = new KPathCoverageGoal(k)
    }
  }

  override def cost(from: DerivationRule): Int = currentGoal.cost(from)

  override def targetReached: Boolean = currentGoal.targetReached

  override def nextTarget(): Boolean = currentGoal.nextTarget()

  override def targetCount: Int = Int.MaxValue

  override def coveredTargets: Int = currentGoal.coveredTargets
}

class PowerSetCoverageGoals(k: Int, p: Int)(implicit grammar: GrammarRepr, random: Random, reach: Reachability) {
  require(p >= 1, s"p must be >= 1 ($p given)")
  private val original = new KPathCoverageGoal(k)

  def goals: Iterator[KPathCoverageGoal] = (1 to original.targets.size).view.flatMap { n =>
    original.targets.subsets(n).take(p).map { ts =>
      val goal = new KPathCoverageGoal(k)
      goal.targets.clear()
      goal.targets ++= ts
      goal
    }
  }.iterator
}
