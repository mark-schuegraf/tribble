package de.cispa.se.tribble
package transformation

/**
  * Eliminates useless symbols, i.e., nongenerating and unreachable symbols.
  *
  * This transformation is special insofar as it is intended to be used as a cleanup post-processing step.
  * Using it in isolation does not work, because the model assembler does not allow unreachable symbols and hangs on nongenerating symbols.
  *
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := "" | 'A,
  * 'B := C
  * </pre>
  *
  * 'A is nongenerating and 'B is unreachable. The grammar will thus be transformed into
  *
  * <pre>
  * 'S := ""
  * </pre>
  *
  * @see the test suites for deletion and unit rule elimination, since their transformers implicate useless symbol elimination
  */
object UselessSymbolElimination extends GrammarTransformer {

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    eliminateUselessSymbols(grammar)
  }

  /** Canonical two-step approach to removing useless symbols: those variables or terminals that do not appear in any derivation of a terminal string from the start symbol.
    *
    * @return Grammar without useless := nongenerating and unreachable symbols.
    * @see Hopcroft et al.'s Introduction to Automata Theory, Languages, and Computation, pp. 262 ff.
    * @example grammar: <pre> 'S := "a" | 'A ~ 'B, 'A := "b" </pre>
    *          step 1, prune nongenerating: <pre> 'S := "a", 'A := "b" </pre>
    *          step 2, prune useless: <pre> 'S := "a" </pre> */
  def eliminateUselessSymbols(grammar: GrammarRepr): GrammarRepr = {
    val generating = eliminateNonGeneratingSymbols(grammar)
    eliminateUnreachableSymbols(generating)
  }

  /** @example simplifies <pre> 'S := "" | 'A, 'A := 'B, 'B := 'A  </pre> to <pre> 'S := "" </pre> */
  def eliminateNonGeneratingSymbols(grammar: GrammarRepr): GrammarRepr = {
    val generatingReferences = getGeneratingReferences(grammar)

    def simplifyRule(rule: DerivationRule): Option[DerivationRule] = rule match {
      case Alternation(alts, id) =>
        /* Discard nongenerating alternatives */
        val newAlts = alts map simplifyRule collect { case Some(rule) => rule }
        if (newAlts.isEmpty) None else if (newAlts.tail.isEmpty) Some(newAlts.head) else Some(Alternation(newAlts, id))
      case c@Concatenation(elements, _) =>
        /* Discard nongenerating concatenations altogether */
        val containsNonGenerating = elements exists (simplifyRule(_).isEmpty)
        if (containsNonGenerating) None else Some(c)
      case Quantification(subject, min, max, id) =>
        /* Discard quantifications with nongenerating subjects */
        val newSubject = simplifyRule(subject)
        if (newSubject.isEmpty) None else Some(Quantification(newSubject.get, min, max, id))
      case r@Reference(name, _) => if (generatingReferences contains name) Some(r) else None
      case _ => Some(rule)
    }

    val simplifiedRules = grammar.rules mapValues simplifyRule collect {
      case (grammar.start, None) => throw new IllegalStateException(s"Nongenerating symbol removal eliminated the start production in: $grammar")
      case (nonTerminal, Some(rule)) => (nonTerminal, rule)
    }
    GrammarRepr(grammar.start, simplifiedRules)
  }

  /** @example simplifies <pre> 'S := "" | 'A, 'B := C </pre> to <pre> 'S := "" | 'A </pre> */
  def eliminateUnreachableSymbols(grammar: GrammarRepr): GrammarRepr = {
    /* A reference is unreachable if it is never used. */
    GrammarRepr(grammar.start, filterUsedReferences(grammar.rules, grammar.start))
  }

  /** Moved from RuleInlining
    *
    * @return grammar without unreachable symbols
    * @author Nikolas Havrikov */
  def filterUsedReferences(rules: Map[NonTerminal, DerivationRule], startSymbol: NonTerminal): Map[NonTerminal, DerivationRule] = {
    var fixpoint = false
    var filteredRules = rules
    do {
      val usedReferences = Set(startSymbol) ++ filteredRules.values.flatMap(_.toStream.flatMap { case r: Reference => Some(r) case _ => None }).map(_.name)
      val newFilteredRules = filteredRules.filterKeys(usedReferences)
      if (newFilteredRules.keySet == filteredRules.keySet)
        fixpoint = true
      filteredRules = newFilteredRules

    } while (!fixpoint)
    filteredRules
  }

}
