package de.cispa.se.tribble
package transformation
package normal_forms

import scala.collection.mutable

/**
  * Eliminates unit rules. <br>
  *
  * requires: preprocessed grammar in Backus-Naur form  <br>
  *
  * 1. Identify unit pairs, which are pairs of variables reachable via a sequence of unit rule applications.  <br>
  * 2. Build a new grammar: For each unit pair (A, B), add a production 'A := grammar(B) filterNot isUnitProduction
  * 3. Prune useless symbols from the grammar to produce a valid output grammar, i.e., eliminate nongenerating or unreachable symbols  <br>
  *
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A,
  * 'A := 'B | 'C ~ 'D,
  * 'B := 'D,
  * 'C := "c1",
  * 'D := "d1" | 'B
  * </pre>
  *
  * The unit pairs are: (S, S), (S, A), (S, B), (S, D), (A, A), (A, B), (A, D), (B, D), (C, C), (D, D), (D, B).  <br>
  * Thus, this new grammar is constructed
  *
  * <pre>
  * 'S := "d1" | 'C ~ 'D,
  * 'A := "d1" | 'C ~ 'D,
  * 'B := "d1",
  * 'C := "c1",
  * 'D := "d1"
  * </pre>
  *
  * Finally, the useless symbols are eliminated. Both 'A and 'B are unreachable, so we obtain
  *
  * <pre>
  * 'S := "d1" | 'C ~ 'D,
  * 'C := "c1",
  * 'D := "d1"
  * </pre>
  */
object UnitRuleElimination extends BNFGrammarTransformer {

  private type UnitPair = (NonTerminal, NonTerminal)

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val unitPairs: Set[UnitPair] = scanUnitPairs(grammar)
    val unitFree: GrammarRepr = eliminateUnitRules(grammar, unitPairs)
    UselessSymbolElimination.transformGrammarNoIds(unitFree)
  }

  private def scanUnitPairs(grammar: GrammarRepr): Set[UnitPair] = {
    val unitPairs = mutable.Set[UnitPair]()
    /* Unit reachability is a reflexive relation, so initialize unit pair set with pairs {(A, A) | A in grammar.variables)}. */
    for (nonTerminal <- grammar.rules.keys) unitPairs add(nonTerminal, nonTerminal)
    /* Then determine nontrivial unit pairs. */
    grammar.rules foreach { case (nonTerminal, rule) => expandUnitRules(nonTerminal, rule, grammar, unitPairs) }
    unitPairs.toSet
  }

  /** Expands the grammar, choosing only unit rule alternatives. <br>
    * Side effect: updates unitPairs with nontrivial unit pairs whose first element is nonTerminal, i.e.: <br>
    * {(nonTerminal, B) | B reachable by unit rules from nonTerminal}
    *
    * @param unitPairs the set of trivial unit pairs to be expanded with nontrivial unit pairs */
  private def expandUnitRules(nonTerminal: NonTerminal, rule: DerivationRule, grammar: GrammarRepr, unitPairs: mutable.Set[UnitPair]): Unit = rule match {
    case Alternation(alternatives, _) => alternatives foreach (expandUnitRules(nonTerminal, _, grammar, unitPairs))
    case Reference(name, _) if !(unitPairs contains(nonTerminal, name)) =>
      unitPairs add(nonTerminal, name)
      expandUnitRules(nonTerminal, grammar(name), grammar, unitPairs)
    case _ =>
  }

  /** Builds a new grammar from the given grammar.
    * Copies nonunit rules from the target to the source productions of the variables in a unit pair (source, target).
    *
    * @param grammar   from which to source the productions
    * @param unitPairs that dictate copy operations
    * @return grammar free from unit rules
    */
  private def eliminateUnitRules(grammar: GrammarRepr, unitPairs: Set[UnitPair]): GrammarRepr = {
    val newRulesAsSets = mutable.Map[NonTerminal, mutable.Set[DerivationRule]]()
    for ((source, target) <- unitPairs) {
      val targetRule = grammar(target)
      val unitFreeTargetRule = pruneUnitAlternatives(targetRule)
      unitFreeTargetRule.foreach { it =>
        if (!newRulesAsSets.isDefinedAt(source)) newRulesAsSets(source) = mutable.Set[DerivationRule]()
        newRulesAsSets(source) add copyWithoutId(it)
      }
    }
    val newRules = newRulesAsSets filter { case (_, v) => v.nonEmpty } mapValues { v => flattenRule(if (v.size == 1) v.head else Alternation(v.toSeq)) }
    GrammarRepr(grammar.start, newRules.toMap)
  }

  private def pruneUnitAlternatives(rule: DerivationRule): Option[DerivationRule] = rule match {
    case Alternation(alts, id) =>
      val newAlts = alts filterNot isUnitRule
      if (newAlts.isEmpty) None else if (newAlts.tail.isEmpty) Some(newAlts.head) else Some(Alternation(newAlts, id))
    case _: Reference => None
    case _ => Some(rule)
  }

}
