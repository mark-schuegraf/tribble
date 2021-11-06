package de.cispa.se.tribble
package transformation
package grammar_adaptation

import scala.collection.mutable

/**
  * Expand [[Quantification]]s into top-level [[Alternation]] rules: <br>
  * The goal is to skew the distribution of repetitions towards encouraging fewer repetitions.
  * Takes each possible [[Quantification]] binding (min,max) and turns it into an alternative for an [[Alternation]] rule, prioritizing fewer repetitions.
  * Then delegate to the [[Alternation]] rule from the locus of the quantification under expansion via a [[Reference]]. <br>
  * Do so until the parametric [[repetitionLimit]] for the number of repetitions is reached, or until all possibilities have been exhausted.
  * In the event that the [[repetitionLimit]] is reached before dissolution of the [[Quantification]],
  * create an additional alternative containing the min-truncated original [[Quantification]]. This ensures language preservation. <br>
  * Why not create separate rules for each repetition?
  * Due to the limitation that a [[Reference]] may occur only on the left-hand side of one production rule,
  * the repetitions need to be delegated to via an [[Alternation]]. <br>
  *
  * <p>
  * An example grammar
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D | 'E | 'F,
  * 'A := "a1".?,
  * 'B := "b1".*,
  * 'C := "c1".+,
  * 'D := "d1".rep(1,2),
  * 'E := "e1".* ~ 'C | 'D,
  * 'F := "f1".* ~ ("f2".* | 'A).+
  * </pre>
  *
  * For [[repetitionLimit]] = 2, this will be transformed into
  *
  * <pre>
  * 'S := 'A | 'B | 'C | 'D | 'E,
  * 'A := 'A_qx0,
  * 'A_qx0 := "" | "a1",
  * 'B := 'B_qx0,
  * 'B_qx0 := "" | "b1" | "b1".rep(2,),
  * 'C := 'C_qx0,
  * 'C_qx0 := "c1" | "c1" ~ "c1" | "c1".rep(3,),
  * 'D := 'D_qx0,
  * 'D_qx0 := "d1" | "d1" ~ "d1",
  * 'E := 'E_qx0 ~ 'C | 'D,
  * 'E_qx0 := "" | "e1" | "e1".rep(2,),
  * 'F := 'F_qx0 ~ 'F_qx1,
  * 'F_qx0 := "" | "f1" | "f1".rep(2,),
  * 'F_qx1 := ('F_qx1_qx0 | 'A) | ('F_qx1_qx0 | 'A) ~ ('F_qx1_qx0 | 'A) | ('F_qx1_qx0 | 'A).rep(3),
  * 'F_qx1_qx0 := "" | "f2" | "f2".rep(2,)
  * </pre>
  *
  * @param repetitionLimit how many times to expand the quantification
  */
class QuantificationExpansion(private val repetitionLimit: Int) extends GrammarTransformer {

  private type Grammar = mutable.Map[NonTerminal, DerivationRule]

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    require(repetitionLimit >= 0, s"The number of repetitions must not be negative! ($repetitionLimit given")

    val newRules: Grammar = mutable.Map[NonTerminal, DerivationRule]()
    /* ids for new rules */
    val ids = mutable.Map[NonTerminal, Int]() withDefaultValue 0
    for ((nonTerminal, rule) <- grammar.rules) {
      expand(nonTerminal, rule)(newRules, ids)
    }
    grammar.copy(rules = newRules.toMap)
  }

  /** Expands [[Quantification]]s in the original grammar up to the [[repetitionLimit]].
    * Recurse on encapsulating productions and invoke [[foldQuantificationPartially]] on any thus encountered [[Quantification]]s. <br>
    * Side effect: updates the temporary grammar with expanded rules.
    *
    * @param nonTerminal identifying the input production rule
    * @param rule        possibly containing [[Quantification]]s
    * @param grammar     to contain the expanded rule
    * @param ids         to track the number of occurrences of [[Quantification]]s in the input rule
    * @return the possibly rewritten [[DerivationRule]] for the given nonTerminal.
    */
  private def expand(nonTerminal: NonTerminal, rule: DerivationRule)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = rule match {
    /* Replace nested quantifications inside quantifications, alternations, and concatenations with references to their expansions. */
    case q: Quantification =>
      val qExpanded = foldQuantificationPartially(nonTerminal, q)
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

  /** Partially folds [[Quantification]] into an [[Alternation]] to be referenced in its place.
    * Therein, each repetition of the [[Quantification]] subject is a separate alternative.
    * Creates up to [[repetitionLimit]] many alternatives, as well as possibly one final rule to allow for infinite production.
    * Note that the new [[Alternation]] rule is not assigned an id.
    * [[Alternation]]s with one or less alternatives are returned directly as their underlying subjects. <br>
    * Side effect: updates the grammar by adding the newly created [[DerivationRule]].
    *
    * @return a [[Reference]] to the newly created partially folded [[Quantification]] rule
    */
  private def foldQuantificationPartially(nonTerminal: NonTerminal, q: Quantification)(implicit grammar: Grammar, ids: mutable.Map[NonTerminal, Int]): DerivationRule = q match {
    case Quantification(subject, min, max, _) =>
      /* Name the newly encountered quantification, so that it may later be referenced accordingly. */
      val index = ids(nonTerminal)
      ids(nonTerminal) += 1
      val name = s"${nonTerminal}_qx$index"
      /* Expand its subject before replication. */
      val expandedSubject: DerivationRule = expand(name, subject)
      /* After the grammar graph has been fully expanded and nested quantifications have been resolved,
         finally create the alternation from the now quantification-free subject and add a corresponding rule to the
         grammar. Return the reference to this rule so that its representation in other rules is the reference, not
         the rule itself. */
      val alternatives: Seq[DerivationRule] = generateAlternativesIteratively(expandedSubject, min, max)
      /* To preserve the generated language, finalize the alternation by possibly adding an out-of-budget alternative
         that generates the remaining quantification bindings. */
      val newMax = min + repetitionLimit - 1
      val truncate: Boolean = newMax < max
      val finalizedAlts = if (truncate) alternatives :+ truncateQuantification(expandedSubject, newMax + 1, max) else alternatives
      val foldedQuantification = if (finalizedAlts.length == 1) finalizedAlts.head else Alternation(finalizedAlts)
      grammar(name) = foldedQuantification
      Reference(name)
  }

  /** Generates the alternatives of an [[Alternation]] that represents an expanded [[Quantification]],
    * i.e., generate sequences of the [[Quantification]] subject from length min to max.
    * Do so until there is nothing left to expand, or the [[repetitionLimit]] has been reached,
    * paying 1 action from the budget for each expansion, even epsilon.
    *
    * @return a sequence containing self-[[Concatenation]]s of the [[Quantification]] subject of lengths min to max
    */
  def generateAlternativesIteratively(subject: DerivationRule, min: Int, max: Int): Seq[DerivationRule] = {
    val alts = mutable.ListBuffer[DerivationRule]()
    var budget: Int = repetitionLimit
    for (repNum <- min to max if budget > 0) {
      repNum match {
        case 0 =>
          alts.prepend(Literal(""))
        case 1 =>
          alts.prepend(copyWithoutId(subject))
        case _ =>
          val reps = mutable.ListBuffer[DerivationRule]()
          for (_ <- 1 to repNum) {
            reps.prepend(copyWithoutId(subject))
          }
          val concat = Concatenation(reps.toList)
          alts.prepend(concat)
      }
      budget -= 1
    }
    alts.toList
  }

  private def truncateQuantification(subject: DerivationRule, newMin: Int, max: Int): DerivationRule = {
    if (newMin == max) copyWithoutId(subject) else Quantification(copyWithoutId(subject), newMin, max)
  }

}
