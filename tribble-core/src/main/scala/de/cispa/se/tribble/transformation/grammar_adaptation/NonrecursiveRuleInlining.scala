package de.cispa.se.tribble
package transformation
package grammar_adaptation

/**
  * Inlines rules into their reference sites, unless they are directly or indirectly recursive.
  *
  * <p>E.g.:
  * <blockquote><pre>
  * 'A := "a" ~ 'A
  * </pre></blockquote>
  * will remain unchanged. Similarly
  * <blockquote><pre>
  * 'A := "a" ~ 'B
  * 'B := "a" ~ 'A
  * </pre></blockquote>
  * and
  * <blockquote><pre>
  * 'A := 'B
  * 'B := 'C
  * 'C := 'A
  * </pre></blockquote>
  * will remain unchanged.
  *
  * Note: will try to preserve rule ids.
  *
  * @param inlineLevels how many times to repeat the inline operation.
  * @param productionFilter optional: inline only those references that fulfil this predicate
  */
class NonrecursiveRuleInlining(private val inlineLevels: Int, private val productionFilter: NonTerminal => Boolean = _ => true) extends RuleInlining(inlineLevels, productionFilter) {
  require(inlineLevels >= 0, s"The number of inline levels must not be negative! ($inlineLevels given")

  override def inlineGrammar(grammar: GrammarRepr): GrammarRepr = {
    val cyclicReferences = getCyclicReferences(grammar)
    // inline references
    val inlinedRules = grammar.rules.mapValues(inlineIfAcyclic(_)(grammar, cyclicReferences)).view.force
    // filter out unused declarations
    GrammarRepr(grammar.start, UselessSymbolElimination.filterUsedReferences(inlinedRules, grammar.start))
  }

  private def inlineIfAcyclic(rule: DerivationRule)(implicit grammar: GrammarRepr, cyclicReferences: Set[NonTerminal]): DerivationRule = rule match {
    case r@Reference(name, _) if cyclicReferences contains name => r
    case _ => inlineAcyclicRule(rule)(grammar, cyclicReferences)
  }

  private def inlineAcyclicRule(rule: DerivationRule)(implicit grammar: GrammarRepr, seenNonTerms: Set[NonTerminal]): DerivationRule = rule match {
    case r@Reference(name, _) if productionFilter(name) => grammar(r)
    case Concatenation(elements, id) => Concatenation(elements.map(inlineIfAcyclic(_)(grammar, seenNonTerms)), id)
    case Alternation(alts, id) => Alternation(alts.map(inlineIfAcyclic(_)(grammar, seenNonTerms)), id)
    case Quantification(subject, min, max, id) => Quantification(inlineIfAcyclic(subject)(grammar, seenNonTerms), min, max, id)
    case rule: TerminalRule => rule
  }

}
