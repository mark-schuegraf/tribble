package de.cispa.se.tribble
package transformation
package normal_forms

import dk.brics.automaton.Automaton

import scala.collection.mutable

/**
  * Eliminates nonroot deletion rules. <br>
  *
  * requires: preprocessed grammar in Backus-Naur form  <br>
  * side-effect: modifies the automata of nullable regexes <br>
  * warning: the underlying regular expression of a nullable regex is not modified, only its automaton.
  * Modifications to regexes are thus not persisted in the output grammar file, unless the "marshal" storing strategy is used. <br>
  *
  * 1. Identify nullable productions by tracking use of terminal deletion rules A := ""  <br>
  * 2. Eliminate nullable productions by creating variants of them without some nullable symbols. <br>
  * 3. Prune all deletion rules, as their use has now been simulated.
  * 4. If any production was nullable, add a nullable start production S := "" to preserve the input language  <br>
  * 5. Prune useless symbols from the grammar to produce a valid output grammar, i.e., eliminate nongenerating or unreachable symbols  <br>
  *
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A,
  * 'A := "a1" ~ 'A ~ 'B | "",
  * 'B := 'C ~ 'C | 'E,
  * 'C := 'D,
  * 'D := "",
  * 'E := "e1"
  * </pre>
  *
  * 'S, 'A, 'B, 'C, and 'D are all nullable, due to the propagation of "" through references to 'C.
  * Thus, we replace their productions with variants where nullable symbols are missing, i.e., every symbol except 'D:
  *
  * <pre>
  * 'S := "" |'A,
  * 'A := "a1" ~ 'A ~ 'B | "a1" ~ 'A | "a1" ~ 'B | "a1"
  * 'B := 'C ~ 'C | 'C | 'C | 'E
  * 'C := "" | 'D,
  * 'D := "",
  * 'E := "e1",
  * 'G := 'D
  * </pre>
  *
  * We now prune all deletions and add a nullable start production S := "", since there were nullable rules:
  *
  * <pre>
  * 'S := "" |'A,
  * 'A := "a1" ~ 'A ~ 'B | "a1" ~ 'A | "a1" ~ 'B | "a1"
  * 'B := 'C ~ 'C | 'C | 'C | 'E
  * 'C := 'D,
  * 'E := "e1"
  * 'G := 'D
  * </pre>
  *
  * Finally, the useless symbols are eliminated. 'D is nongenerating and thus transitively also 'C. 'G is unreachable:
  *
  * <pre>
  * 'S := "" |'A,
  * 'A := "a1" ~ 'A ~ 'B | "a1" ~ 'A | "a1" ~ 'B | "a1"
  * 'B := 'E
  * 'E := "e1"
  * </pre>
  */
object DeletionRuleElimination extends BNFGrammarTransformer {

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val nullableFree: GrammarRepr = substituteDeletionRules(grammar)
    UselessSymbolElimination.transformGrammarNoIds(nullableFree)
  }

  /** @return grammar without top-level epsilons, except possibly a single language-preserving epsilon derived from the start symbol */
  private def filterNonStartEpsilons(grammar: GrammarRepr): GrammarRepr = {
    require(!containsConcatenationsWithEpsilonElements(grammar), s"epsilon filtering cannot deal with epsilon concatenation elements in: $grammar")

    /* Discard 'A := "" */
    val terminalDeletionFreeRules = grammar.rules filterNot { case (_, rule) => isTerminalDeletionRule(rule) }
    /* Filter "" from 'A := "" | ... */
    val alternationDeletionFreeRules: Map[NonTerminal, DerivationRule] = terminalDeletionFreeRules map { case (nonTerminal, rule) =>
      rule match {
        case Alternation(alts, id) =>
          val newAlts = alts filterNot isTerminalDeletionRule
          val newRule = if (newAlts.tail.nonEmpty) Alternation(newAlts, id) else newAlts.head
          (nonTerminal, newRule)
        case _ => (nonTerminal, rule)
      }
    }
    /* If the unfiltered grammar contains a start symbol deletion rule, add it to the filtered grammar */
    val startProduction = grammar(grammar.start)
    val existsStartEpsilon: Boolean = isTerminalDeletionRule(startProduction) || isAlternationDeletionRule(startProduction)
    val rulesWithStartDeletion: Map[NonTerminal, DerivationRule] =
      if (existsStartEpsilon) {
        val newDel =
          if (alternationDeletionFreeRules contains grammar.start) {
            alternationDeletionFreeRules(grammar.start) match {
              case Alternation(alts, id) => Alternation(Literal("") +: alts, id)
              case rule: DerivationRule => Alternation(Seq(Literal(""), rule))
            }
          } else Literal("")
        alternationDeletionFreeRules + (grammar.start -> newDel)
      } else alternationDeletionFreeRules
    GrammarRepr(grammar.start, rulesWithStartDeletion)
  } ensuring(!containsNonStartDeletionRules(_), s"epsilon filtering did not remove all nonstart deletion rules")

  /** @return variants of [grammar.rules] without nullable symbols, unless they derive from [grammar.start] */
  private def substituteDeletionRules(grammar: GrammarRepr): GrammarRepr = {
    val nullableReferences = getNullableReferences(grammar)

    /** Side effect: updates [newRules] with variants of [rule].
      *
      * @return variants of [rule] that differ in the presence of nullable symbols
      * @example assume B, C nullable: <pre> 'S := A ~ B ~ C </pre> becomes <pre> 'S := A | A ~ B | A ~ C | A ~ B ~ C </pre> */
    def subSomeNullableReferences(nonTerminal: NonTerminal, rule: DerivationRule)(grammar: GrammarRepr, newRules: mutable.Map[NonTerminal, DerivationRule]): Seq[DerivationRule] = rule match {
      case Alternation(alts, id) =>
        /* Gather all collections of alternative variants, then empty them out. */
        val newAlts = alts flatMap (subSomeNullableReferences(nonTerminal, _)(grammar, newRules))
        /* Merge internal alternations if possible. */
        val newA = flattenRule(Alternation(newAlts, id))
        newRules(nonTerminal) = newA
        Seq(newA)
      case Concatenation(elements, _) =>
        /* Generate cross product of the elements' variants collections. */
        val elemVariants: Seq[Seq[DerivationRule]] = elements map (subSomeNullableReferences(nonTerminal, _)(grammar, newRules))
        val epsilonFreeCrossProduct = (xs: Seq[DerivationRule], ys: Seq[DerivationRule]) =>
          for {x <- xs; y <- ys} yield (x, y) match {
            /* Merge binary epsilons. */
            case (Literal("", _), Literal("", _)) => x
            case (Literal("", _), r2: Regex) if r2.automaton.isEmptyString => x
            case (r1: Regex, Literal("", _)) if r1.automaton.isEmptyString => y
            case (r1: Regex, r2: Regex) if r1.automaton.isEmptyString && r2.automaton.isEmptyString => Literal("")
            /* Consume unary epsilons. */
            case (_, Literal("", _)) => x
            case (_, r2: Regex) if r2.automaton.isEmptyString => x
            case (Literal("", _), _) => y
            case (r1: Regex, _) if r1.automaton.isEmptyString => y
            /* Merge concatenations in non-nested fashion. */
            case _ => flattenRule(Concatenation(Seq(x, y)))
          }
        val zero: Seq[DerivationRule] = Seq(Literal(""))
        val newConcs: Seq[DerivationRule] = elemVariants.fold(zero)(epsilonFreeCrossProduct) map copyWithoutId
        newRules(nonTerminal) = if (newConcs.tail.isEmpty) newConcs.head else Alternation(newConcs)
        newConcs
      case r@Reference(name, _) if nullableReferences contains name =>
        /* Each occurrence of a nullable reference must be duplicated without that reference. */
        val refVariants: Seq[DerivationRule] = Seq(Literal(""), r)
        newRules(nonTerminal) = Alternation(refVariants)
        refVariants
      case r: Regex if isNullableTerminal(r) =>
        /* Split the regex into an epsilon literal and its complement's intersection with the automaton. */
        r.automaton = r.automaton.minus(Automaton.makeEmptyString)
        newRules(nonTerminal) = r
        Seq(Literal(""), r)
      case _ =>
        newRules(nonTerminal) = rule
        Seq(rule)
    }

    /* Create variants of rules that simulate deriving any of its nullable references to epsilon. */
    val replacedRules = mutable.Map[NonTerminal, DerivationRule]()
    for ((nonTerminal, rule) <- grammar.rules) {
      subSomeNullableReferences(nonTerminal, rule)(grammar, replacedRules)
    }
    val replacedGrammar = GrammarRepr(grammar.start, replacedRules.toMap)
    assert(!containsConcatenationsWithEpsilonElements(replacedGrammar), s"grammar must have no epsilons in concatenations after substitution: $replacedGrammar")
    /* Now remove stray epsilons from the grammar, except if they derive from the start symbol. */
    filterNonStartEpsilons(replacedGrammar)
  } ensuring(!containsNestedConstructs(_), s"rule replacement must not introduce nesting in violation of BNF")

  /** Assertion helper */
  private def containsNonStartDeletionRules(grammar: GrammarRepr): Boolean = {
    val nonStartGrammar = grammar.rules - grammar.start
    val containsTerminalDeletionRules = nonStartGrammar.values exists isTerminalDeletionRule
    val containsAlternationDeletionRules = nonStartGrammar.values exists isAlternationDeletionRule
    containsTerminalDeletionRules || containsAlternationDeletionRules
  }

  /** Assertion helper */
  private def containsConcatenationsWithEpsilonElements(grammar: GrammarRepr): Boolean = {
    grammar.rules.values exists {
      case Concatenation(elements, _) => elements exists isTerminalDeletionRule
      case _ => false
    }
  }

  /** Assertion helper, requires BNF */
  private def containsNestedConstructs(grammar: GrammarRepr): Boolean = {
    def outerExpand(rule: DerivationRule): Boolean = rule match {
      case Alternation(alternatives, _) => alternatives forall {
        case Concatenation(elements, _) => elements forall innerExpand
        case alt: DerivationRule => outerExpand(alt)
      }
      case Concatenation(elements, _) => elements forall innerExpand
      case _ => false
    }

    def innerExpand(rule: DerivationRule): Boolean = rule match {
      case Alternation(_, _) | Concatenation(_, _) => false
      case _ => true
    }

    grammar.rules.values forall outerExpand
  }

}
