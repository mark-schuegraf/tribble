package de.cispa.se.tribble
package transformation
package normal_forms

import scala.collection.mutable

/**
  * Eliminates terminal symbols except in right hand sides of size 1 <br>
  *
  * requires: preprocessed grammar in Backus-Naur form  <br>
  *
  * For every nonsolitary [[TerminalRule]] "a" in a rule body:  <br>
  *
  * 1. Introduce a new nonterminal 'N_a  <br>
  * 2. Replace the terminal "a" with its corresponding nonterminal 'N_a  <br>
  * 3. Add the production 'N_a := "a"  to the grammar  <br>
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D,
  * 'A := "a1",
  * 'B := "a1" ~ 'E,
  * 'C := 'E ~ "a1" ~ 'F,
  * 'D := 'E ~ "a1" ~ "a2" ~ 'F
  * </pre>
  *
  * The rule for 'B, 'C, and 'D contain nonsolitary terminals.
  * The grammar will hence be transformed into
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D,
  * 'A := "a1",
  * 'B := 'B_term0 ~ 'E,
  * 'B_term0 := "a1"
  * 'C := 'E ~ 'C_term0 ~ 'F,
  * 'C_term0 := "a1"
  * 'D := 'E ~ 'D_term1 ~ 'D_term2 ~ 'F
  * 'D_term1 := "a1"
  * 'D_term2 := "a2"
  * </pre>
  */
object NonsolitaryTerminalExtraction extends BNFGrammarTransformer {

  private type Grammar = mutable.Map[NonTerminal, DerivationRule]

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val newRules: Grammar = mutable.Map[NonTerminal, DerivationRule]()
    /* ids for new rules */
    val ids = mutable.Map[NonTerminal, Int]() withDefaultValue 0
    for ((nonTerminal, rule) <- grammar.rules) {
      extractNonsolitaryTerminals(nonTerminal, rule)(newRules, ids)
    }
    grammar.copy(rules = newRules.toMap)
  }

  /** Finds and replaces each nonsolitary [[TerminalRule]] in the original grammar.
    * Recurse on encapsulating productions and invoke [[foldTerminal]] on any thus encountered [[TerminalRule]]. <br>
    * Side effect: updates the temporary grammar with new rules.
    *
    * @param nonTerminal identifying the input production rule
    * @param rule        possibly containing a nonsolitary [[TerminalRule]]
    * @param grammar     to contain the expanded rule
    * @param ids         to track the number of occurrences of a [[TerminalRule]] in a production
    * @return the possibly rewritten [[DerivationRule]] for the given nonTerminal.
    */
  private def extractNonsolitaryTerminals(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    /* Recurse into top-level alternations. */
    case Alternation(alts, id) =>
      val aExpanded = Alternation(alts.map(extractNonsolitaryTerminals(nonTerminal, _)), id)
      grammar(nonTerminal) = aExpanded
      aExpanded
    /* Terminals inside concatenations are necessarily not isolated, so extract them. */
    case Concatenation(elements, id) =>
      val cExpanded = Concatenation(elements.map(foldTerminal(nonTerminal, _)), id)
      grammar(nonTerminal) = cExpanded
      cExpanded
    /* Leave chain rules (references) and solitary terminal rules (literals and regexes) intact . */
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }

  /** Folds each [[TerminalRule]] inside a [[Concatenation]] into a new terminal production to be referenced in its place. <br>
    * Side effect: updates the grammar by adding the newly created terminal [[DerivationRule]].
    *
    * @return a [[Reference]] to the newly created rule
    */
  private def foldTerminal(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    /* Rule is a terminal rule, so fold */
    case _: Literal | _: Regex =>
      val index = ids(nonTerminal)
      ids(nonTerminal) += 1
      val name = s"${nonTerminal}_term$index"
      grammar(name) = rule
      Reference(name)
    /* Rule is a reference, so retain */
    case _ =>
      rule
  }

}
