package de.cispa.se.tribble

import de.cispa.se.tribble.DerivationRule.DEFAULT_ID
import de.cispa.se.tribble.transformation.UselessSymbolElimination
import dk.brics.automaton.Automaton
import dk.brics.automaton.BasicOperations.determinize

import scala.collection.mutable

/** Contains utility methods for use by a [[GrammarTransformer]] */
package object transformation {

  /** Deep copies a [[DerivationRule]], discarding the previous [[DerivationRule.id]].
    * Required for the creation of new grammar constructs during transformation. */
  def copyWithoutId(rule: DerivationRule): DerivationRule = rule match {
    case a@Alternation(alts, _) => a.copy(alts.map(copyWithoutId), DEFAULT_ID)
    case c@Concatenation(elements, _) => c.copy(elements.map(copyWithoutId), DEFAULT_ID)
    case q@Quantification(subject, _, _, _) => q.copy(copyWithoutId(subject), id = DEFAULT_ID)
    case r: Reference => r.copy(id = DEFAULT_ID)
    case l: Literal => l.copy(id = DEFAULT_ID)
    case x: Regex =>
      val newX = x.copy(id = DEFAULT_ID)
      newX.automaton = x.automaton
      newX
  }

  /** @param grammar that may contain nongenerating references
    * @return references that are part of a cyclical derivation */
  def getCyclicReferences(grammar: GrammarRepr): Set[NonTerminal] = {
    def isCyclic(nonTerminal: NonTerminal, rule: DerivationRule)(grammar: GrammarRepr, seenNonTerms: mutable.Map[NonTerminal, Int]): Boolean = {
      seenNonTerms(nonTerminal) += 1
      rule match {
        case Reference(name, _) =>
          /* Undefined symbols are considered acyclic. */
          if (!(grammar.rules contains name)) false
          else if (seenNonTerms.getOrElse(name, 0) < 2) isCyclic(name, grammar(name))(grammar, seenNonTerms)
          else true
        case Alternation(alternatives, _) => alternatives exists (isCyclic(nonTerminal, _)(grammar, seenNonTerms))
        case Concatenation(elements, _) =>
          /* copy map for concatenations so case A -> BB is not labeled as cyclical */
          elements exists { elem =>
            val seenNonTermsCopy = (mutable.Map[NonTerminal, Int]() withDefaultValue 0) ++ seenNonTerms
            isCyclic(nonTerminal, elem)(grammar, seenNonTermsCopy)
          }
        case Quantification(subject, _, _, _) => isCyclic(nonTerminal, subject)(grammar, seenNonTerms)
        case _: TerminalRule => false
      }
    }

    val cyclicRefs = mutable.Set[NonTerminal]()
    grammar.rules map { case (k, v) =>
      val seenNonTerms = mutable.Map[NonTerminal, Int]() withDefaultValue 0
      if (isCyclic(k, v)(grammar, seenNonTerms)) cyclicRefs add k
    }
    cyclicRefs.toSet
  }

  /** @return whether the given rule can be derived to a [[TerminalRule]] that satisfies the condition */
  private def canDeriveSatisfactoryTerminal(rule: DerivationRule, conditionToSatisfy: TerminalRule => Boolean)(grammar: GrammarRepr, cyclicReferences: Set[NonTerminal]): Boolean = {
    def deriveAndCheck(rule: DerivationRule, conditionToSatisfy: TerminalRule => Boolean)(grammar: GrammarRepr, unusedCyclicReferences: Map[NonTerminal, Boolean]): Boolean = rule match {
      case Alternation(alternatives, _) => alternatives exists (deriveAndCheck(_, conditionToSatisfy)(grammar, unusedCyclicReferences))
      case Concatenation(elements, _) => elements forall (deriveAndCheck(_, conditionToSatisfy)(grammar, unusedCyclicReferences))
      case Quantification(subject, min, _, _) =>
        deriveAndCheck(subject, conditionToSatisfy)(grammar, unusedCyclicReferences) ||
          /* Simulate deletion quantification derivation. */
          (if (min == 0) deriveAndCheck(Literal(""), conditionToSatisfy)(grammar, unusedCyclicReferences) else false)
      case Reference(name, _) =>
        /* Undefined symbols never lead to a terminal. */
        if (!(grammar.rules contains name)) false
        /* Follow cyclic paths once only. */
        else if (unusedCyclicReferences contains name) {
          if (unusedCyclicReferences(name)) {
            val updatedCyclicReferences = unusedCyclicReferences updated(name, false)
            deriveAndCheck(grammar(name), conditionToSatisfy)(grammar, updatedCyclicReferences)
          } else false
          /* Always resolve acyclic references*/
        } else deriveAndCheck(grammar(name), conditionToSatisfy)(grammar, unusedCyclicReferences)
      case t: TerminalRule => conditionToSatisfy(t)
    }

    val unusedCyclicReferences = (cyclicReferences zipAll(Set(), "", true)).toMap
    deriveAndCheck(rule, conditionToSatisfy)(grammar, unusedCyclicReferences)
  }

  /** @return references that are part of a terminal derivation */
  def getGeneratingReferences(grammar: GrammarRepr): Set[NonTerminal] = {
    val cyclicRefs = getCyclicReferences(grammar)
    val generatingRefs = mutable.Set[NonTerminal]()
    grammar.rules map { case (k, v) =>
      if (canDeriveSatisfactoryTerminal(v, isGeneratingTerminal)(grammar, cyclicRefs)) {
        generatingRefs add k
      }
    }
    generatingRefs.toSet
  }

  /** @return references that are part of an epsilon derivation */
  def getNullableReferences(grammar: GrammarRepr): Set[NonTerminal] = {
    val cyclicRefs = getCyclicReferences(grammar)
    val nullableRefs = mutable.Set[NonTerminal]()
    grammar.rules map { case (k, v) =>
      if (canDeriveSatisfactoryTerminal(v, isNullableTerminal)(grammar, cyclicRefs)) {
        nullableRefs add k
      }
    }
    nullableRefs.toSet
  }

  /** Checks if an input grammar conforms to Backus-Naur form. */
  def isInBackusNaurForm(grammar: GrammarRepr): Boolean = {
    def checkRuleConformityOuter(rule: DerivationRule): Boolean = rule match {
      case Alternation(alternatives, _) =>
        /* Concatenations just below top level are allowed */
        alternatives.forall((rule: DerivationRule) =>
          if (rule.isInstanceOf[Concatenation])
            checkRuleConformityOuter(rule)
          else
            checkRuleConformityInner(rule))
      case Concatenation(elements, _) => elements.forall(checkRuleConformityInner)
      case _: Quantification => false
      case _ => true
    }

    def checkRuleConformityInner(rule: DerivationRule): Boolean = rule match {
      case _: Alternation | _: Concatenation | _: Quantification => false
      case _ => true
    }

    grammar.rules.values.forall(checkRuleConformityOuter)
  }

  /** @param grammar in Backus-Naur form required for correct result  */
  def isChomskyRule(nonTerminal: NonTerminal, rule: DerivationRule, grammar: GrammarRepr): Boolean = rule match {
    case Alternation(alternatives, _) => alternatives forall (isChomskyRule(nonTerminal, _, grammar))
    case Concatenation(elements, _) => (elements.size < 3) &&
      (elements forall {
        /* Nongenerating symbols are illegal. */
        case Reference(name, _) if grammar.rules contains name => true
        case _ => false
      })
    case _: Reference => false
    case t: TerminalRule if nonTerminal != grammar.start => !isEpsilonTerminal(t)
    case _ => true
  }

  /** Checks if an input grammar conforms to Chomsky normal form. */
  def isInChomskyNormalForm(grammar: GrammarRepr): Boolean = {
    /* bnf && reachable && termBinDelUnitNongenerating */
    isInBackusNaurForm(grammar) &&
      (grammar.rules == UselessSymbolElimination.filterUsedReferences(grammar.rules, grammar.start)) &&
      (grammar.rules forall { case (k, v) => isChomskyRule(k, v, grammar) })
  }

  /** @param grammar in Backus-Naur form required for correct result  */
  def isExtendedChomskyRule(nonTerminal: NonTerminal, rule: DerivationRule, grammar: GrammarRepr): Boolean = isUnitRule(rule) || isChomskyRule(nonTerminal, rule, grammar)

  /** Checks if an input grammar conforms to extended Chomsky normal form. */
  def isInExtendedChomskyNormalForm(grammar: GrammarRepr): Boolean = {
    /* bnf && reachable && termBinDelNongenerating */
    isInBackusNaurForm(grammar) &&
      (grammar.rules == UselessSymbolElimination.filterUsedReferences(grammar.rules, grammar.start)) &&
      (grammar.rules forall { case (k, v) => isExtendedChomskyRule(k, v, grammar) })
  }

  /** Flattens every adjacent internal [[Alternation]] and [[Concatenation]]
    *
    * @example flattens <pre> S' := 'A ~ ('B ~ 'C) </pre> but not <pre> S' := 'A ~ ('B | (C' ~ 'D) </pre> */
  def flattenRule(rule: DerivationRule): DerivationRule = {
    def flatten(r: DerivationRule): DerivationRule = r match {
      case Alternation(alts, id) =>
        val newAlts = alts flatMap {
          case Alternation(subAlts, _) => subAlts
          case x => Seq(x)
        }
        Alternation(newAlts, id)
      case Concatenation(elements, id) =>
        val newElems = elements flatMap {
          case Concatenation(subElems, _) => subElems
          case x => Seq(x)
        }
        Concatenation(newElems, id)
      case q: Quantification => q.copy(flatten(q.subject))
      case _ => rule
    }

    var prevRule = rule
    var nextRule = flatten(rule)
    while (prevRule != nextRule) {
      prevRule = nextRule
      nextRule = flatten(prevRule)
    }
    nextRule
  }

  def isGeneratingTerminal(t: TerminalRule): Boolean = t match {
    case r: Regex if r.automaton.isEmpty => false
    case _ => true
  }

  def matchesEpsilon(a: Automaton): Boolean = {
    determinize(a)
    a.getInitialState.isAccept
  }

  def isNullableTerminal(t: TerminalRule): Boolean = t match {
    case Literal("", _) => true
    case r: Regex if matchesEpsilon(r.automaton) => true
    case _ => false
  }

  def isEpsilonTerminal(t: TerminalRule): Boolean = t match {
    case Literal("", _) => true
    case r: Regex => r.automaton.isEmptyString
    case _ => false
  }

  def isTerminalDeletionRule(rule: DerivationRule): Boolean = rule match {
    case t: TerminalRule => isEpsilonTerminal(t)
    case _ => false
  }

  def isAlternationDeletionRule(rule: DerivationRule): Boolean = rule match {
    case Alternation(alternatives, _) => alternatives exists isTerminalDeletionRule
    case _ => false
  }

  def isUnitRule(rule: DerivationRule): Boolean = rule match {
    case _: Reference => true
    case _ => false
  }

}
