package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.model.OrderedGrammarRepr

import scala.collection.mutable

/**
  * Linearizes left recursion. <br>
  * Turns leftmost references into forward references, thereby eliminating mutual left recursion and left-recursive self-loops. <br>
  *
  * requires: preprocessed grammar in at least extended Chomsky normal form <br>
  *
  * Traverses the production set in the topological ordering of the grammar graph:  <br>
  * A. Case external alternation 'A_i := ... | ... => recurse on alternatives <br>
  * B. Case terminal rule 'A_i := "a1" => continue  <br>
  * C. Case concatenation 'A_i := 'A_j ~ ... => case distinction: <br>
  * <ul>
  * <li> i &lt j: continue </li>
  * <li> i > j: potential mutual left recursion. Apply lemma 4.6.1. Repeat </li>
  * <li> i = j: left-recursive self-loop. Apply lemma 4.6.2 </li>
  * </ul>
  * Both lemmas may produce useless symbols, particularly is unit rules are present, i.e., the input grammar is in extended Chomsky normal form.
  * Thus, finally useless symbols are pruned from the grammar. <br>
  *
  * <p>
  *
  * Now follows an example taken from Harrison pp. 113, 114.
  * Note that it is missing an unreferenced start production and the variable names are not accurate.
  * For example, 'Z_1 would be named 'A_2_lrec here.
  *
  * "Consider G as shown below:
  *
  * <pre>
  * 'A_1 := 'A_2 ~ 'A_3,
  * 'A_2 := "1" | 'A_1 ~ 'A_2,
  * 'A_3 := "0" | 'A_1 ~ 'A_3
  * </pre>
  *
  * Applying lemma 4.6.1 to 'A_2 yields:
  *
  * <pre>
  * 'A_1 := 'A_2 ~ 'A_3,
  * 'A_2 := "1" | 'A_2 ~ 'A_3 ~ 'A_2,
  * 'A_3 := "0" | 'A_1 ~ 'A_3
  * </pre>
  *
  * Now lemma 4.6.2 can be applied to the second production:
  *
  * <pre>
  * 'A_1 := 'A_2 ~ 'A_3,
  * 'A_2 := "1" | "1" ~ 'Z_1,
  * 'Z_1 := 'A_3 ~ 'A_2 | 'A_3 ~ 'A_2 ~ 'Z_1,
  * 'A_3 := "0" | 'A_1 ~ 'A_3
  * </pre>
  *
  * Focussing on 'A_3, we use Lemma 4.6.1 to obtain:
  *
  * <pre>
  * 'A_1 := 'A_2 ~ 'A_3,
  * 'A_2 := "1" | "1" ~ 'Z_1,
  * 'Z_1 := 'A_3 ~ 'A_2 | 'A_3 ~ 'A_2 ~ 'Z_1,
  * 'A_3 := "0" | 'A_2 ~ 'A_3 ~ 'A_3
  * </pre>
  *
  * Now lemma 4.6.1 is used again on 'A_3:
  *
  * <pre>
  * 'A_1 := 'A_2 ~ 'A_3,
  * 'A_2 := "1" | "1" ~ 'Z_1,
  * 'Z_1 := 'A_3 ~ 'A_2 | 'A_3 ~ 'A_2 ~ 'Z_1,
  * 'A_3 := "0" | "1" ~ 'A_3 ~ 'A_3 | "1" ~ 'Z_1 ~ 'A_3 ~ 'A_3
  * </pre>
  *
  * Now it is possible to begin the process of back-substitution." <br>
  *
  * The grammar is now free of mutual left recursion and left-recursive self-loops, but may still contain leading nonterminals and thus be left-recursive.
  *
  * @see Harrison's Introduction to Formal Language Theory, pp. 111 ff. <br>
  *      The key lemmas 4.6.1 and 4.6.2 can be found directly on p. 111.
  */
object LeftRecursionLinearization extends OrderedECNFGrammarTransformer {

  override def transformGrammarWithOrderNoIds(orderedGrammar: OrderedGrammarRepr): GrammarRepr = {
    val newRules = mutable.Map[NonTerminal, DerivationRule]() ++ orderedGrammar.grammar.rules
    orderedGrammar.orderedProductions foreach { case (nonTerm, rule) => linearizeProduction(nonTerm, rule, orderedGrammar, newRules) }
    val linearized = GrammarRepr(orderedGrammar.grammar.start, newRules.toMap)
    UselessSymbolElimination.transformGrammarNoIds(linearized)
  }

  /** Three step approach that linearizes the 'A_i := ... production:
    * 1. Repeatedly substitue 'A_j in rules 'A_i := 'A_j ... with i > j until all rules have i &lt j.
    * 2. Replace all rules 'A_i := 'A_j ... with i = j with right-recursive variants.
    * Now i < j for all productions.
    *
    * Side-effect: updates [newRules] with the possibly changed [rule] and also possibly a new production 'nonTerminal_lrec produced during self-loop resolution. */
  private def linearizeProduction(nonTerminal: NonTerminal, rule: DerivationRule, orderedG: OrderedGrammarRepr, newRules: mutable.Map[NonTerminal, DerivationRule]): Unit = {
    /* 1. Acyclic substitution. Turns potential mutual left recursion into left-recursive self-loops. */
    var fixpoint = false
    var substitutedRule = rule
    do {
      val newSubstitutedRule = substituteLeftmostReferences(nonTerminal, substitutedRule, orderedG, newRules)
      if (newSubstitutedRule == substitutedRule) {
        fixpoint = true
      }
      substitutedRule = newSubstitutedRule
    } while (!fixpoint)
    /* 2. Self-loop resolution. Turns left-recursive self-loops into right recursion. */
    newRules(nonTerminal) = resolveSelfLoops(nonTerminal, substitutedRule, orderedG, newRules)
  }

  /** If [rule] beings with a reference and that reference comes before [nonterminal] in the ordering, substitute its definition into [rule].
    *
    * @see Lemma 4.6.1 in Harrison's Introduction to Formal Language Theory, p. 111 */
  private def substituteLeftmostReferences(nonTerminal: NonTerminal, rule: DerivationRule, orderedGrammar: OrderedGrammarRepr, newRules: mutable.Map[NonTerminal, DerivationRule]): DerivationRule = rule match {
    case Alternation(alts, id) =>
      val newAlts = alts map (substituteLeftmostReferences(nonTerminal, _, orderedGrammar, newRules))
      flattenRule(Alternation(newAlts, id))
    case c@Concatenation(elements, id) =>
      val newHead = elements.head match {
        case Reference(name, _) =>
          val (i, j) = (orderedGrammar(nonTerminal), orderedGrammar(name))
          if (i > j) Some(newRules(name)) else None
        case _ => None
      }
      if (newHead.isEmpty) c else newHead.get match {
        case Alternation(alts, _) =>
          val newAlts = alts map (alt => copyWithoutId(flattenRule(Concatenation(alt +: elements.tail))))
          Alternation(newAlts)
        case _ => flattenRule(Concatenation(copyWithoutId(newHead.get) +: elements.tail, id))
      }
    case Reference(name, _) =>
      val (i, j) = (orderedGrammar(nonTerminal), orderedGrammar(name))
      if (i > j) newRules(name) else rule
    case _ => rule
  }

  /** If [rule] begins with a reference and that reference matches [nonterminal],
    * replace [rule] with a set of right-recursive productions consisting of a variant of itself and a new right-recursive helper production named [nonterminal]_lrec. <br>
    * Side effect: adds a helper production to [newRules] if there is any self-recursion to resolve.
    *
    * @see Lemma 4.6.2 in Harrison's Introduction to Formal Language Theory, p. 111 */
  private def resolveSelfLoops(nonTerminal: NonTerminal, rule: DerivationRule, orderedGrammar: OrderedGrammarRepr, newRules: mutable.Map[NonTerminal, DerivationRule]): DerivationRule = {
    val beginsWithSelfReference: DerivationRule => Boolean = {
      case Concatenation(elements, _) => elements.head match {
        case Reference(name, _) if orderedGrammar(nonTerminal) == orderedGrammar(name) => true
        case _ => false
      }
      case Reference(name, _) if orderedGrammar(nonTerminal) == orderedGrammar(name) => true
      case _ => false
    }
    val discardFirstElem: DerivationRule => Option[DerivationRule] = {
      case Concatenation(elements, id) => if (elements.tail.size > 1) Some(Concatenation(elements.tail, id)) else Some(elements.tail.head)
      case _ => None
    }
    val alphas = mutable.ListBuffer[DerivationRule]()
    val betas = mutable.ListBuffer[DerivationRule]()
    rule match {
      case Alternation(alts, _) =>
        alphas appendAll (alts filter beginsWithSelfReference map discardFirstElem collect { case Some(rule) => rule })
        betas appendAll (alts filterNot beginsWithSelfReference)
      case c: Concatenation =>
        if (beginsWithSelfReference(c)) {
          val alpha = discardFirstElem(c)
          if (alpha.isDefined) alphas append alpha.get
        } else betas append c
      case _ => if (!beginsWithSelfReference(rule)) betas append rule
    }
    if (alphas.nonEmpty) {
      /* Rule contains self-loops, so replace. */
      val helperRuleName = s"${nonTerminal}_lrec"
      val helperAlts = alphas ++ (alphas map { alpha => flattenRule(Concatenation(Seq(alpha, Reference(helperRuleName)))) })
      val helperRule = if (helperAlts.size > 1) Alternation(helperAlts.toList) else helperAlts.head
      newRules(helperRuleName) = helperRule
      val newAlts = betas ++ (betas map { beta => flattenRule(Concatenation(Seq(beta, Reference(helperRuleName)))) })
      if (newAlts.size > 1) Alternation(newAlts.toList) else newAlts.head
    } else {
      /* Rule contains either no self-loops or an isolated unit self-loop 'A := 'A, so keep rule intact but discard unit self-loop. */
      rule match {
        case Alternation(_, id) => Alternation(betas.toList, id)
        case _ => betas.toList.head
      }
    }
  }

}
