package de.cispa.se.tribble
package transformation

import de.cispa.se.tribble.input.{AssignIds, CheckIds}
import de.cispa.se.tribble.model.OrderedGrammarRepr
import de.cispa.se.tribble.transformation.grammar_adaptation._
import de.cispa.se.tribble.transformation.normal_forms._

/* Houses simple transformations' transformers, i.e., those that are trivial or merely reference other transformations. */

/** Requires valid ids. */
trait GrammarTransformer {
  def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    CheckIds.process(grammar)
    val gVariant = transformGrammarNoIds(grammar)
    AssignIds.process(gVariant)
  }

  def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr
}

/** Requires input grammar in BNF. */
trait BNFGrammarTransformer extends GrammarTransformer {
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    require(isInBackusNaurForm(grammar), s"Attempted to transform grammar not in Backus-Naur form: $grammar")
    super.transformGrammar(grammar)
  }
}

/** Requires input grammar in CNF. */
trait CNFGrammarTransformer extends BNFGrammarTransformer {
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    require(isInChomskyNormalForm(grammar), s"Attempted to transform grammar not in Chomsky normal form: $grammar")
    super.transformGrammar(grammar)
  }
}

/** Requires input grammar in extended CNF. */
trait ECNFGrammarTransformer extends BNFGrammarTransformer {
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    require(isInExtendedChomskyNormalForm(grammar), s"Attempted to transform grammar not in extended Chomsky normal form: $grammar")
    super.transformGrammar(grammar)
  }
}

/** Requires ordering on the grammar. */
trait OrderedNoIdTransformer {
  def transformGrammarWithOrderNoIds(orderedGrammar: OrderedGrammarRepr): GrammarRepr
}

/** Requires input grammar in CNF and an ordering on the grammar. */
trait OrderedCNFGrammarTransformer extends CNFGrammarTransformer with OrderedNoIdTransformer {
  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val orderedGrammar = OrderedGrammarRepr(grammar)
    transformGrammarWithOrderNoIds(orderedGrammar)
  }
}

/** Requires input grammar in extended CNF and an ordering on the grammar. */
trait OrderedECNFGrammarTransformer extends ECNFGrammarTransformer with OrderedNoIdTransformer {
  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val orderedGrammar = OrderedGrammarRepr(grammar)
    transformGrammarWithOrderNoIds(orderedGrammar)
  }
}

object IdentityTransformer extends GrammarTransformer {
  /** No need for id checks, as the grammar is not modified.
    *
    * @return the input grammar */
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = transformGrammarNoIds(grammar)

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = grammar
}

object DuplicateAlternativeElimination extends GrammarTransformer {
  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    /* Reset ids to ensure duplicate alternative checking ignores ids. */
    grammar.rules.values.flatMap(_.toStream).view.force.foreach(_.id = DerivationRule.DEFAULT_ID)
    val newRules = grammar.rules mapValues {
      case Alternation(alts, id) => Alternation(alts.distinct, id)
      case rule: DerivationRule => rule
    }
    GrammarRepr(grammar.start, newRules)
  }
}

/** Desugars a grammar in tribble format by: <br>
  * 1. Eliminating every internal [[Alternation]] and [[Concatenation]] to achieve EBNF <br>
  * 2. Eliminating every [[Quantification]] to achieve BNF <br>
  */
object BackusNaurFormalizer extends GrammarTransformer {
  /** No need for id checks, as the elementary transformers guarantee valid ids.
    *
    * @return grammar in Backus-Naur form */
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    transformGrammarNoIds(grammar)
  }

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val ebnfPart = AlternationExtraction.transformGrammar(grammar)
    val ebnf = ConcatenationExtraction.transformGrammar(ebnfPart)
    val bnf = QuantificationElimination.transformGrammar(ebnf)
    bnf
  }
}

/** @see Follows elementary transformation ordering by Lange and Leiß: "To CNF or not to CNF? An Efficient Yet Presentable Version of the CYK Algorithm" */
object ChomskyFormalizer extends BNFGrammarTransformer {
  /** No need for id checks, as the elementary transformers guarantee valid ids.
    *
    * @return grammar in Chomsky normal form */
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    transformGrammarNoIds(grammar)
  }

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val term = NonsolitaryTerminalExtraction.transformGrammar(grammar)
    val bin = NonbinaryRuleReduction.transformGrammar(term)
    val del = DeletionRuleElimination.transformGrammar(bin)
    val unit = UnitRuleElimination.transformGrammar(del)
    unit
  }
}

/** @see Follows elementary transformation ordering by Lange and Leiß: "To CNF or not to CNF? An Efficient Yet Presentable Version of the CYK Algorithm" */
object ExtendedChomskyFormalizer extends BNFGrammarTransformer {
  /** No need for id checks, as the elementary transformers guarantee valid ids.
    *
    * @return grammar in Chomsky normal form */
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    transformGrammarNoIds(grammar)
  }

  override def transformGrammarNoIds(grammar: GrammarRepr): GrammarRepr = {
    val term = NonsolitaryTerminalExtraction.transformGrammar(grammar)
    val bin = NonbinaryRuleReduction.transformGrammar(term)
    val del = DeletionRuleElimination.transformGrammar(bin)
    del
  }
}

/** Eliminates left recursion.
  * @see Follows proof by Harrison: Introduction to Formal Language Theory, pp. 111 ff.*/
object GreibachFormalizer extends OrderedCNFGrammarTransformer {
  /** No need for id checks, as the elementary transformers guarantee valid ids.
    *
    * @return grammar in Chomsky normal form */
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    transformGrammarNoIds(grammar)
  }

  override def transformGrammarWithOrderNoIds(orderedGrammar: OrderedGrammarRepr): GrammarRepr = {
    val linearized = LeftRecursionLinearization.transformGrammarWithOrderNoIds(orderedGrammar)
    val leftRecursionFree = LeadingNonterminalElimination.process(linearized, orderedGrammar)
    leftRecursionFree
  }
}

/** Eliminates left recursion except in unit rules that follow the topological ordering of the grammar graph.
  * @see Follows proof by Harrison: Introduction to Formal Language Theory, pp. 111 ff.*/
object ExtendedGreibachFormalizer extends OrderedECNFGrammarTransformer {
  /** No need for id checks, as the elementary transformers guarantee valid ids.
    *
    * @return grammar in Chomsky normal form */
  override def transformGrammar(grammar: GrammarRepr): GrammarRepr = {
    transformGrammarNoIds(grammar)
  }

  override def transformGrammarWithOrderNoIds(orderedGrammar: OrderedGrammarRepr): GrammarRepr = {
    val linearized = LeftRecursionLinearization.transformGrammarWithOrderNoIds(orderedGrammar)
    val unitRuleLeftRecursive = UnitRulePreservingLeadingNonterminalElimination.process(linearized, orderedGrammar)
    unitRuleLeftRecursive
  }
}
