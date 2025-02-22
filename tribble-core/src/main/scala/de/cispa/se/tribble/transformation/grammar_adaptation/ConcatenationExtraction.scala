package de.cispa.se.tribble
package transformation
package grammar_adaptation

import scala.collection.mutable

/**
  * Extracts all internal concatenations into top level: <br>
  * Folds every encountered internal concatenation into a new top level rule.
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A | 'B,
  * 'A := "a1" ~ ('D ~ "a2").? ~ ("a3" ~ ('D ~ "a4" ~ "a5")).rep,
  * 'B := "b1" | "b2" ~ ("b3" ~ ('D ~ 'C ~ "b4") ~ 'C),
  * 'C := "c1" | "c2" ~ ("c3" ~ "c4"),
  * 'D := "d1" | "d2"
  * </pre>
  *
  * will be transformed into
  *
  * <pre>
  * 'S := 'A | 'B,
  * 'A := "a1" ~ 'A_a0.? ~ ("a3" ~ 'A_a1).rep,
  * 'A_a0 := 'D ~ "a2",
  * 'A_a1 :=  'D ~ "a4" ~ "a5",
  * 'B := "b1" | "b2" ~ 'B_a0,
  * 'B_a0 := "b3" ~ 'B_a0_a0 ~ 'C,
  * 'B_a0_a0 := 'D ~ 'C ~ "b4",
  * 'C := "c1" | "c2" ~ 'C_a0,
  * 'C_a0 := "c3" ~ "c4",
  * 'D := "d1" | "d2"
  * </pre>
  *
  */
object ConcatenationExtraction extends GrammarTransformer {

  private type Grammar = mutable.Map[NonTerminal, DerivationRule]

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val newRules: Grammar = mutable.Map[NonTerminal, DerivationRule]()
    val ids = mutable.Map[NonTerminal, Int]() withDefaultValue 0
    for ((nonTerminal, rule) <- grammar.rules) {
      outerExtract(nonTerminal, rule)(newRules, ids)
    }
    GrammarRepr(grammar.start, rules = newRules.toMap)
  }

  /**
    * Extracts every [[Concatenation]] from a top-level grammar production.
    * This method will NOT create new references for alternations encountered on top level.
    * Side effect: updates the grammar with newly created sub-rules.
    *
    * @return the possibly rewritten derivation rule for the given nonTerminal.
    */
  private def outerExtract(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    case Alternation(alts, id) =>
      val alt = Alternation(alts.map(outerExtract(nonTerminal, _)), id)
      grammar(nonTerminal) = alt
      alt
    case c: Concatenation =>
      val newC = Concatenation(c.elements.map(innerExtract(nonTerminal, _)), c.id)
      grammar(nonTerminal) = newC
      newC
    case Quantification(subject, min, max, id) =>
      val q = Quantification(innerExtract(nonTerminal, subject), min, max, id)
      grammar(nonTerminal) = q
      q
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }

  /**
    * Extracts every [[Concatenation]] from an internal derivation rule.
    * This method will create new references for all encountered concatenations.
    * Side effect: updates the grammar with newly created sub-rules.
    *
    * @return the possibly rewritten derivation rule for the given nonTerminal.
    */
  private def innerExtract(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    case Alternation(alts, id) =>
      val alt = Alternation(alts.map(innerExtract(nonTerminal, _)), id)
      grammar(nonTerminal) = alt
      alt
    case c: Concatenation =>
      val index = ids(nonTerminal)
      ids(nonTerminal) += 1
      val name = s"${nonTerminal}_c$index"
      val r = Reference(name)
      grammar(nonTerminal) = r
      val newC = Concatenation(c.elements.map(innerExtract(name, _)))
      grammar(name) = newC
      r
    case Quantification(subject, min, max, id) =>
      val q = Quantification(innerExtract(nonTerminal, subject), min, max, id)
      grammar(nonTerminal) = q
      q
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }
}
