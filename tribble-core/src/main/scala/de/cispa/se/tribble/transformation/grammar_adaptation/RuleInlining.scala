package de.cispa.se.tribble
package transformation
package grammar_adaptation

/**
  * Inlines rules into their reference sites.
  *
  * <p>E.g.:
  * <blockquote><pre>
  * 'A := "a" ~ 'B ~ 'C,
  * 'B := "b" ~ 'C,
  * 'C := "c"
  * </pre></blockquote>
  * after [[inlineLevels]] = 1 will be transformed into
  * <blockquote><pre>
  * 'A := "a" ~ "b" ~ 'C ~ "c",
  * 'C := "c"
  * </pre></blockquote>
  * and if [[inlineLevels]] is > 1, it will turn into
  * <blockquote><pre>
  * 'A := "a" ~ "b" ~ "c" ~ "c"
  * </pre></blockquote>
  * <p>
  * When a rule is recursive, it will be inlined [[inlineLevels]] times and the original rule will not be removed. E.g.:
  * <blockquote><pre>
  * 'A := "" | "a" ~ 'A
  * </pre></blockquote>
  * will become
  * <blockquote><pre>
  * 'A := "" | "a" ~ ("" | "a" ~ 'A)
  * </pre></blockquote>
  * <p> When there are mutually recursive rules, both will be inlined in each other in a random order. E.g.:
  * <blockquote><pre>
  * 'A := "a" ~ 'B,
  * 'B := "b" ~ 'A
  * </pre></blockquote>
  * Might turn into
  * <blockquote><pre>
  * 'A := "a" ~ "b" ~ "a" ~ 'B,
  * 'B := "b" ~ "a" ~ 'B
  * </pre></blockquote>
  *
  * Note: will try to preserve rule ids.
  *
  * @param inlineLevels how many times to repeat the inline operation.
  * @param productionFilter optional: inline only those references that fulfil this predicate
  */
class RuleInlining(private val inlineLevels: Int, private val productionFilter: NonTerminal => Boolean = _ => true) extends GrammarTransformer {
  require(inlineLevels >= 0, s"The number of inline levels must not be negative! ($inlineLevels given")

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    var g = grammar
    for (_ <- 0 until inlineLevels) {
      g = inlineGrammar(g)
    }
    g
  }

  protected def inlineGrammar(grammar: GrammarRepr): GrammarRepr = {
    // inline references
    val inlinedRules = grammar.rules.mapValues(inlineRule(_)(grammar)).view.force
    // filter out unused declarations
    GrammarRepr(grammar.start, UselessSymbolElimination.filterUsedReferences(inlinedRules, grammar.start))
  }

  private def inlineRule(rule: DerivationRule)(implicit grammar: GrammarRepr): DerivationRule = rule match {
    case r@ Reference(name, _) if productionFilter(name) => grammar(r)
    case Concatenation(elements, id) => Concatenation(elements.map(inlineRule), id)
    case Alternation(alts, id) => Alternation(alts.map(inlineRule), id)
    case Quantification(subject, min, max, id) => Quantification(inlineRule(subject), min, max, id)
    case rule: TerminalRule => rule
  }

}
