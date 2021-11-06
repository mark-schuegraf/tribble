package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._
import de.cispa.se.tribble.input.{ModelAssembler, SharedAutomatonCache}

class DeletionRuleEliminationSpec extends TransformationTestSpecification with SharedAutomatonCache {

  "Deletion rule elimination" should "inline terminal deletion rules" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B | 'C,
      'B := "b1",
      'C := ""
    )
    val eG = Grammar(
      'S := "" | 'A,
      'A := 'B,
      'B := "b1",
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "propagate deletions in alternation deletion rules through chain rules" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := "" | "b1"
    )
    val eG = Grammar(
      'S := "" | 'A,
      'A := 'B,
      'B := "b1"
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "consume epsilons in concatenations during rule replacement" in {
    val g = Grammar(
      'S := 'A ~ 'B ~ "s1" ~ 'C,
      'A := "",
      'B := "\"\"".regex,
      'C := ""
    )
    val eG = Grammar(
      'S := "s1"
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "extract single-element concatenations' contents during rule replacement" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "",
      'B := "b1"
    )
    val eG = Grammar(
      'S := 'B,
      'B := "b1"
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "prune duplicate epsilon alternatives if derived from the start symbol" in {
    val g = Grammar(
      'S := "" | "" | "s1"
    )
    val eG = Grammar(
      'S := "" | "s1"
    )
    /* Bypass the shared model assembler here, because it does not allow the first grammar to be constructed. */
    val duplicateIgnoringAssembler = new ModelAssembler(automatonCache, assignProbabilities = false, checkDuplicateAlternatives = false)
    checkGrammarEquality(DeletionRuleElimination, g, eG, duplicateIgnoringAssembler)
  }

  it should "deal with self-loops" in {
    val g = Grammar(
      'S := 'A | 'B,
      'A := "" | 'A,
      'B := "b1" | 'B
    )
    val eG = Grammar(
      'S := "" | 'B,
      'B := "b1" | 'B
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "deal with external mutual recursion" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := "" | 'A
    )
    val eG = Grammar(
      'S := ""
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "deal with internal mutual recursion" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B ~ 'B,
      'B := "" | 'A ~ 'A
    )
    val eG = Grammar(
      'S := ""
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "deal with empty regexes" in {
    val g = Grammar(
      'S := "s1" | 'A,
      'A := "~(.*)".regex
    )
    val eG = Grammar(
      'S := "s1"
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
  }

  it should "deal with nullable regexes" in {
    val g = Grammar(
      'S := 'A,
      'A := "[0-9]*".regex
    )
    val eG = Grammar(
      'S := "" | 'A,
      'A := "[0-9]*".regex
    )
    checkGrammarEquality(DeletionRuleElimination, g, eG)
    automatonShouldNotMatchEpsilon("A", g)
  }

  private def automatonShouldNotMatchEpsilon(nonTerminal: NonTerminal, grammar: Grammar): Unit = {
    val original = modelAssembler.assemble(grammar.productions)
    val transformed = DeletionRuleElimination.transformGrammar(original)
    transformed(nonTerminal) match {
      case r: Regex => if (matchesEpsilon(r.automaton)) throw new IllegalStateException(s"the regex's automaton should no longer accept epsilon but does: $r.automaton")
      case _ => throw new IllegalStateException(s"the regex was unexpectedly transformed to another grammar construct")
    }
  }

}
