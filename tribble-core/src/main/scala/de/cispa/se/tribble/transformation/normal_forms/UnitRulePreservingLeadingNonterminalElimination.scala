package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.model.OrderedGrammarRepr

import scala.collection.mutable

/**
  * Variant of [[LeadingNonterminalElimination]] that leaves unit rules intact. <br>
  *
  * requires: linearized grammar and the ordering used to do so via [[LeftRecursionLinearization]] <br>
  * warning: does not remove left recursion if the provided ordering differs from the linearization ordering or the grammar has unit rules <br>
  * note: not intended to be used on its own and thus not a [[GrammarTransformer]]
  *
  * Substitutes for leading nonterminals their definitions, unless they are unit rules.
  * In doing so, prioritizes substitution of nonterminals by reversing the ordering on nonterminals used during linearization.
  * Whenever a rule is substituted that was previously referenced only by leftmost nonterminals, that rule becomes unreachable.
  * Thus, finally useless symbols are pruned from the grammar. <br>
  *
  * Ignoring unit rules has the effect that the substitution of their nonterminal body into other leftmost reference sites causes their nonterminals not to be eliminated by substitution.
  * Affected rules must subsequently be processed again, in order that their leftmost nonterminals take one link along the chain that leads to the potentially length n unit rule chain to a terminal rule.
  * To accomplish this, contrary to [[LeadingNonterminalElimination]], a fixed-point iteration approach is applied, wherein nonunit rules are processed until their leading nonterminal is eventually eliminated. <br>
  *
  *
  * <p>
  *
  * Consider the example grammar
  *
  * <pre>
  * 'S := 'A ~ 'A,
  * 'A := 'B,
  * 'B := "b1"
  * </pre>
  *
  * Regular [[LeadingNonterminalElimination]] would propagate the final literal to the 'A production and then 'S
  *
  * <pre>
  * 'S := "b1" ~ 'A,
  * 'A := "b1",
  * 'B := "b1"
  * </pre>
  *
  * After useless symbol we get
  *
  * <pre>
  * 'S := "b1" ~ 'A,
  * 'A := "b1"
  * </pre>
  *
  * Note how the grammar now has no unit rules.
  * This transformation is designed to preserve them instead.
  * In the first iteration of the fixed-point approach, this happens to the input grammar
  *
  * <pre>
  * 'S := 'B ~ 'A,
  * 'A := 'B,
  * 'B := "b1"
  * </pre>
  *
  * The second iteration sees that leading nonterminals are removed in nonunit rules
  *
  * <pre>
  * 'S := "b1" ~ 'A,
  * 'A := 'B,
  * 'B := "b1"
  * </pre>
  *
  * There are no useless symbols to remove.
  *
  * @see Harrison's Introduction to Formal Language Theory, pp. 111 ff. <br>
  *      The key lemmas 4.6.1 and 4.6.2 can be found directly on p. 111.
  * @see the test suite for [[ExtendedGreibachFormalizer]], since its transformer implicates [[UnitRulePreservingLeadingNonterminalElimination]]
  */
object UnitRulePreservingLeadingNonterminalElimination {

  /** As long as there are leading nonterminals, substitute in reverse topological order for leading nonterminals in nonunit rules.
    *
    * @param grammar        the output grammar of [[LeftRecursionLinearization]]
    * @param orderedGrammar the ordered input grammar of [[LeftRecursionLinearization]] */
  def process(grammar: GrammarRepr, orderedGrammar: OrderedGrammarRepr): GrammarRepr = {
    var newRules: Map[NonTerminal, DerivationRule] = grammar.rules
    var rules = newRules
    do {
      rules = newRules
      orderedGrammar.orderedProductions.reverseIterator foreach { case (substitutionTarget, _) => newRules = substituteInLinearizedProductions(substitutionTarget, newRules) }
    } while (newRules != rules)
    val leftRecursionFree = GrammarRepr(grammar.start, newRules)
    val duplicateFree = DuplicateAlternativeElimination.transformGrammarNoIds(leftRecursionFree)
    UselessSymbolElimination.transformGrammarNoIds(duplicateFree)
  }

  /** @return the production set with leading [substitutionTarget] nonterminals having been substituted for once in nonunit rules */
  private def substituteInLinearizedProductions(substitutionTarget: NonTerminal, rules: Map[NonTerminal, DerivationRule]): Map[NonTerminal, DerivationRule] = {
    val newRules = mutable.Map[NonTerminal, DerivationRule]() ++ rules
    rules foreach { case (nonTerminal, rule) => backSubstituteLeftmostNonterminalUnlessUnitRule(substitutionTarget, nonTerminal, rule, newRules) }
    newRules.toMap
  }

  /** If [rule] is not a unit rule, [rule] begins with a reference, and that reference targets [substitutionTarget], substitute its definition into [rule] and update the entry of [nonterminal] in [newRules]. <br>
    * Side-effect: updates [nonTerminal -> rule] in [newRules] to be [nonTerminal -> back-substituted rule]
    *
    * @see Lemma 4.6.1 in Harrison's Introduction to Formal Language Theory, p. 111 */
  private def backSubstituteLeftmostNonterminalUnlessUnitRule(substitutionTarget: NonTerminal, nonTerminal: NonTerminal, rule: DerivationRule, newRules: mutable.Map[NonTerminal, DerivationRule]): DerivationRule = rule match {
    case Alternation(alts, id) =>
      val newAlts = alts map (backSubstituteLeftmostNonterminalUnlessUnitRule(substitutionTarget, nonTerminal, _, newRules))
      val newA = flattenRule(Alternation(newAlts, id))
      newRules(nonTerminal) = newA
      newA
    case c@Concatenation(elements, id) =>
      val newHead = elements.head match {
        case Reference(name, _) if name == substitutionTarget => Some(newRules(substitutionTarget))
        case _ => None
      }
      val newC = if (newHead.isEmpty) c else newHead.get match {
        case Alternation(alts, _) =>
          val newAlts = alts map (alt => copyWithoutId(flattenRule(Concatenation(alt +: elements.tail))))
          Alternation(newAlts)
        case _ => flattenRule(Concatenation(copyWithoutId(newHead.get) +: elements.tail, id))
      }
      newRules(nonTerminal) = newC
      newC
    case _ =>
      newRules(nonTerminal) = rule
      rule
  }

}
