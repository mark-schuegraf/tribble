package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.model.OrderedGrammarRepr

import scala.collection.mutable

/**
  * Eliminates leading nonterminals in linearized input grammars, resulting in a grammar free of left recursion. <br>
  *
  * requires: linearized grammar and the ordering used to do so via [[LeftRecursionLinearization]] <br>
  * warning: does not remove left recursion if the provided order differs from the linearization ordering or the grammar has unit rules <br>
  * note: not intended to be used on its own and thus not a [[GrammarTransformer]]
  *
  * Substitutes for leading nonterminals their definitions.
  * In doing so, prioritizes substitution of nonterminals by reversing the ordering on nonterminals used during linearization.
  * Whenever a rule is substituted that was previously referenced only by leftmost nonterminals, that rule becomes unreachable.
  * Thus, finally useless symbols are pruned from the grammar. <br>
  *
  * <p>
  *
  * Continuing the example taken from Harrison pp. 113, 114. presented in [[LeftRecursionLinearization]]: <br>
  *
  * "[...] Now it is possible to begin the process of back-substitution.
  * 'A_3 and 'A_2 are already in Greibach form, but 'A_1 is not:
  *
  * <pre>
  * 'A_1 := "1" ~ 'A_3 | "1" ~ 'Z_1 ~ 'A_3,
  * 'A_2 := "1" | "1" ~ 'Z_1,
  * 'Z_1 := 'A_3 ~ 'A_2 | 'A_3 ~ 'A_2 ~ 'Z_1,
  * 'A_3 := "0" | "1" ~ 'A_3 ~ 'A_3 | "1" ~ 'Z_1 ~ 'A_3 ~ 'A_3
  * </pre>
  *
  * If we substitute for 'A_3 in the 'Z_1 rules, we obtain:
  *
  * <pre>
  * 'A_1 := "1" ~ 'A_3 | "1" ~ 'Z_1 ~ 'A_3,
  * 'A_2 := "1" | "1" ~ 'Z_1,
  * 'Z_1 := "0" ~ 'A_2 | "1" ~ 'A_3 ~ 'A_3 ~ 'A_2 | "1" ~ 'Z_1 ~ 'A_3 ~ 'A_3 ~ 'A_2 | "0" ~ 'A_2 ~ 'Z_1 | "1" ~ 'A_3 ~ 'A_3 ~ 'A_2 ~ 'Z_1 | "1" ~ 'Z_1 ~ 'A_3 ~ 'A_3 ~ 'A_2 ~ 'Z_1,
  * 'A_3 := "0" | "1" ~ 'A_3 ~ 'A_3 | "1" ~ 'Z_1 ~ 'A_3 ~ 'A_3
  * </pre>
  *
  * This grammar is in Greibach form."
  *
  * The grammar now has no more leading nonterminals and is thus free of left recursion.
  *
  * @see Harrison's Introduction to Formal Language Theory, pp. 111 ff. <br>
  *      The key lemmas 4.6.1 and 4.6.2 can be found directly on p. 111.
  * @see the test suite for [[GreibachFormalizer]], since its transformer implicates [[LeadingNonterminalElimination]]
  */
object LeadingNonterminalElimination {

  /** @param grammar        the output grammar of [[LeftRecursionLinearization]]
    * @param orderedGrammar the ordered input grammar of [[LeftRecursionLinearization]] */
  def process(grammar: GrammarRepr, orderedGrammar: OrderedGrammarRepr): GrammarRepr = {
    var newRules: Map[NonTerminal, DerivationRule] = grammar.rules
    orderedGrammar.orderedProductions.reverseIterator foreach { case (substitutionTarget, _) => newRules = substituteInLinearizedProductions(substitutionTarget, newRules) }
    val leftRecursionFree = GrammarRepr(grammar.start, newRules)
    val duplicateFree = DuplicateAlternativeElimination.transformGrammarNoIds(leftRecursionFree)
    UselessSymbolElimination.transformGrammarNoIds(duplicateFree)
  }

  /** @return the production set without leading [substitutionTarget] nonterminals */
  private def substituteInLinearizedProductions(substitutionTarget: NonTerminal, rules: Map[NonTerminal, DerivationRule]): Map[NonTerminal, DerivationRule] = {
    val newRules = mutable.Map[NonTerminal, DerivationRule]() ++ rules
    rules foreach { case (nonTerminal, rule) => backSubstituteLeftmostNonterminal(substitutionTarget, nonTerminal, rule, newRules) }
    newRules.toMap
  }

  /** If [rule] begins with a reference and that reference targets [substitutionTarget], substitute its definition into [rule] and update the entry of [nonterminal] in [newRules]. <br>
    * Side-effect: updates [nonTerminal -> rule] in [newRules] to be [nonTerminal -> back-substituted rule]
    *
    * @see Lemma 4.6.1 in Harrison's Introduction to Formal Language Theory, p. 111 */
  private def backSubstituteLeftmostNonterminal(substitutionTarget: NonTerminal, nonTerminal: NonTerminal, rule: DerivationRule, newRules: mutable.Map[NonTerminal, DerivationRule]): DerivationRule = rule match {
    case Alternation(alts, id) =>
      val newAlts = alts map (backSubstituteLeftmostNonterminal(substitutionTarget, nonTerminal, _, newRules))
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
    case Reference(name, _) if name == substitutionTarget =>
      val newR = newRules(substitutionTarget)
      newRules(nonTerminal) = newR
      newR
    case _ =>
      newRules(nonTerminal) = rule
      rule
  }

}
