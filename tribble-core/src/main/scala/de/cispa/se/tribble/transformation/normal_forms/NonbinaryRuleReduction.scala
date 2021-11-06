package de.cispa.se.tribble
package transformation
package normal_forms

import scala.collection.mutable

/**
  * Reduces nonterminal rules to right-hand sides of size &lt= 2 <br>
  *
  * requires: preprocessed grammar in Backus-Naur form and containing no nonsolitary terminals  <br>
  * If the input grammar also contains no solitary terminals, the new rules will be binary nonterminal rules. <br>
  * Else they will be possibly mixed terminal/nonterminal rules.
  *
  * For every rule 'A with a sequence of [[Reference]]s 'B1 'B2 .. 'Bk, k &gt 2, as its body:  <br>
  * 1. Introduce k-2 new nonterminals 'C1 'C2 ... 'Ck-2  <br>
  * 2. Break the sequence into a cascade of binary productions: <br>
  * <pre>
  * 'A := 'B1 'C1
  * 'C1 := 'B2 'C2
  * ...
  * 'Ck-3 := 'Bk-2 'Ck-2
  * 'Ck-2 := 'Bk-1 'Bk
  * </pre>
  * 3. Replace the original production 'A := 'B1 'B2 .. 'Bk with the cascade of new productions  <br>
  *
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D,
  * 'A := 'D,
  * 'B := 'D ~ 'E,
  * 'C := 'D ~ 'E ~ 'F
  * 'D := "d1",
  * 'E := "e1",
  * 'F := "f1",
  * </pre>
  *
  * The rule for 'C contains a sequence of nonterminals of length 3 &gt 2.
  * The grammar will hence be transformed into
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D,
  * 'A := 'D,
  * 'B := 'D ~ 'E,
  * 'C := 'D ~ 'C_bin0
  * 'C_bin0 := 'E ~ 'F
  * 'D := "d1",
  * 'E := "e1",
  * 'F := "f1",
  * </pre>
  */
object NonbinaryRuleReduction extends BNFGrammarTransformer {

  private type Grammar = mutable.Map[NonTerminal, DerivationRule]

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val newRules: Grammar = mutable.Map[NonTerminal, DerivationRule]()
    /* ids for new rules */
    val ids = mutable.Map[NonTerminal, Int]() withDefaultValue 0
    for ((nonTerminal, rule) <- grammar.rules) {
      reduceNonbinaryRules(nonTerminal, rule)(newRules, ids)
    }
    grammar.copy(rules = newRules.toMap)
  }

  /** Finds and replaces each nonbinary sequence of [[Reference]]s in the original grammar.
    * Recurse on encapsulating productions and invoke [[cascadeConcatenation()]] on any thus encountered [[Concatenation]]. <br>
    * Side effect: updates the temporary grammar with new rules.
    *
    * @param nonTerminal identifying the input production rule
    * @param rule        possibly containing a nonbinary sequence of [[Reference]]s
    * @param grammar     to contain the expanded rule
    * @param ids         to track the number of occurrences of nonbinary sequences in a production
    * @return the possibly rewritten [[DerivationRule]] for the given nonTerminal.
    */
  private def reduceNonbinaryRules(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    /* Recurse into top-level alternations. */
    case Alternation(alts, id) =>
      val aExpanded = Alternation(alts.map(reduceNonbinaryRules(nonTerminal, _)), id)
      grammar(nonTerminal) = aExpanded
      aExpanded
    /* Cascade concatenations. */
    case c: Concatenation =>
      val cReduced = cascadeConcatenation(nonTerminal, c)
      grammar(nonTerminal) = cReduced
      cReduced
    /* Leave chain rules (references) and solitary terminal rules (literals and regexes) intact . */
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }

  /** Cascades every [[Concatenation]] into a set of new binary productions that preserve its semantics. <br>
    * Side effect: updates the grammar by adding the newly created nonterminal [[DerivationRule]]s.
    *
    * @return the possibly rewritten [[Concatenation]]
    */
  private def cascadeConcatenation(nonTerminal: NonTerminal, c: Concatenation)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): Concatenation = {
    def cascadeTail(nonTerminal: NonTerminal, elements: Seq[DerivationRule])(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): Reference = {
      val index = ids(nonTerminal)
      ids(nonTerminal) += 1
      val name = s"${nonTerminal}_bin$index"
      if (elements.length > 2) {
        val cHead = elements.head
        val cTail = cascadeTail(name, elements.tail)
        grammar(name) = new Concatenation(Seq(cHead, cTail))
      } else {
        grammar(name) = new Concatenation(elements)
      }
      Reference(name)
    }
    /* Cascade a concatenation only if it is nonbinary. */
    if (c.elements.length > 2) Concatenation(Seq(c.elements.head, cascadeTail(nonTerminal, c.elements.tail))) else c
  }

}
