package de.cispa.se.tribble
package transformation
package grammar_adaptation

import scala.collection.mutable

/**
  * Eliminate [[Quantification]]s by folding them into a recursive top-level [[Alternation]] rule: <br>
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D | 'E | 'F,
  * 'A := "a1".?,
  * 'B := "b1".*,
  * 'C := "c1".+,
  * 'D := "d1".rep(1,1),
  * 'E := "e1".* ~ 'C | 'D,
  * 'F := "f1".* ~ ("f2".* | 'A).+
  * </pre>
  *
  * This will be transformed into
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D | 'E,
  * 'A := 'A_qe0,
  * 'A_qe0 := "" | "a1",
  * 'B := 'B_qe0,
  * 'B_qe0 := "" | "b1" ~ 'B_qe0,
  * 'C := 'C_qe0,
  * 'C_qe0 := "c1" ~ 'C_qe0,
  * 'D := 'D_qe0,
  * 'D_qe0 := "d1",
  * 'E := 'E_qe0 ~ 'C | 'D,
  * 'E_qe0 := "" | "e1" ~ 'E_qe0,
  * 'F := 'F_qe0 ~ 'F_qe1,
  * 'F_qe0 := "" | "f1" ~ 'F_qe0,
  * 'F_qe1 := ('F_qe1_qe0 | 'A) | ('F_qe1_qe0 | 'A) ~ 'F_qe1,
  * 'F_qe1_qe0 := "" | "f2" ~ 'F_qe1_qe0
  * </pre>
  */
object QuantificationElimination extends GrammarTransformer {

  private type Grammar = mutable.Map[NonTerminal, DerivationRule]

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val newRules: Grammar = mutable.Map[NonTerminal, DerivationRule]()
    /* ids for new rules */
    val ids = mutable.Map[NonTerminal, Int]() withDefaultValue 0
    for ((nonTerminal, rule) <- grammar.rules) {
      expand(nonTerminal, rule)(newRules, ids)
    }
    grammar.copy(rules = newRules.toMap)
  }

  /** Eliminates [[Quantification]]s in the original grammar.
    * Recurse on encapsulating productions and invoke [[foldQuantification]] on any thus encountered [[Quantification]]s. <br>
    * Side effect: updates the temporary grammar with expanded rules.
    *
    * @param nonTerminal identifying the input production rule
    * @param rule        possibly containing [[Quantification]]s
    * @param grammar     to contain the expanded rule
    * @param ids         to track the number of occurrences of [[Quantification]]s in the input rule
    * @return the possibly rewritten [[DerivationRule]] for the given nonTerminal.
    */
  private def expand(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    /* Replace nested quantifications inside quantifications, alternations, and concatenations with references to their recursive representations. */
    case q: Quantification =>
      val qExpanded = foldQuantification(nonTerminal, q)
      grammar(nonTerminal) = qExpanded
      qExpanded
    case Alternation(alts, id) =>
      val aExpanded = Alternation(alts.map(expand(nonTerminal, _)), id)
      grammar(nonTerminal) = aExpanded
      aExpanded
    case Concatenation(elements, id) =>
      val cExpanded = Concatenation(elements.map(expand(nonTerminal, _)), id)
      grammar(nonTerminal) = cExpanded
      cExpanded
    /* Leave literals, references, and regexes intact. */
    case _ =>
      grammar(nonTerminal) = rule
      rule
  }

  /** Folds [[Quantification]] into a new production to be referenced in its place. <br>
    * For a [[Quantification]] subject.rep(min, max), add alternatives to an [[Alternation]] rule using [[generateAlternativesRecursively]].
    * Then create a [[Reference]] to that [[Alternation]] and replace the [[Quantification]] with it. <br>
    * Side effect: updates the grammar by adding the newly created [[DerivationRule]].
    *
    * @return a [[Reference]] to the newly created rule
    */
  private def foldQuantification(nonTerminal: NonTerminal, q: Quantification)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = q match {
    case Quantification(subject, min, max, _) =>
      /* Name the newly encountered quantification, so that it may later be referenced accordingly. */
      val index = ids(nonTerminal)
      ids(nonTerminal) += 1
      val name = s"${nonTerminal}_qe$index"
      /* Expand its subject before replication. */
      val expandedSubject: DerivationRule = expand(name, subject)
      /* After the grammar graph has been fully expanded and nested quantifications have been resolved,
         finally create the alternation from the now quantification-free subject and add a corresponding rule to the
         grammar. Return the reference to this rule so that its representation in other rules is the reference, not
         the rule itself. */
      val alternatives: Seq[DerivationRule] = generateAlternativesRecursively(expandedSubject, min, max, name)
      val foldedQuantification = if (alternatives.length == 1) alternatives.head else Alternation(alternatives)
      grammar(name) = foldedQuantification
      Reference(name)
  }

  /** Generates the alternatives of an [[Alternation]] to represent a [[Quantification]] recursively, if possible.
    * If not possible due to the quantification being finite, fall back to [[QuantificationExpansion]].
    *
    * @param name of the new nonterminal to self-reference in the recursive alternative
    * @return a sequence of alternatives that together represent the input [[Quantification]]
    */
  def generateAlternativesRecursively(subject: DerivationRule, min: Int, max: Int, name: String): Seq[DerivationRule] = {
    val alts = mutable.ListBuffer[DerivationRule]()
    val fallBackToIterativeGeneration = max < Int.MaxValue
    val expansionBudget = if (fallBackToIterativeGeneration) max - min + 1 else 1
    val breakingAlts = new QuantificationExpansion(expansionBudget).generateAlternativesIteratively(subject, min, max)
    breakingAlts foreach (alts.prepend(_))
    /* Add recursive production if quantification is Kleene plus or Kleene star */
    if (!fallBackToIterativeGeneration) {
      val selfRef = Reference(name)
      val atoms = List[DerivationRule](copyWithoutId(subject), selfRef)
      val concat = Concatenation(atoms)
      alts.prepend(concat)
    }
    alts.toList
  }

}
