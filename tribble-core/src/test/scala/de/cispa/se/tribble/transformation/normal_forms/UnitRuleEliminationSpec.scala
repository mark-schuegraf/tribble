package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

class UnitRuleEliminationSpec extends TransformationTestSpecification {

  "Unit rule elimination" should "collapse unit rule cascades" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "b1"
    )
    checkGrammarEquality(UnitRuleElimination, g, eG)
  }

  it should "not use nonterminal concatenations in derivations" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | 'A ~ 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "a1" | 'A ~ 'B,
      'A := "a1" | 'A ~ 'B,
      'B := "b1"
    )
    checkGrammarEquality(UnitRuleElimination, g, eG)
  }

  it should "deal with self-loops" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | 'A
    )
    val eG = Grammar(
      'S := "a1"
    )
    checkGrammarEquality(UnitRuleElimination, g, eG)
  }

  it should "deal with mutual recursion" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | 'B,
      'B := "b1" | 'A
    )
    val eG = Grammar(
      'S := "a1" | "b1"
    )
    checkGrammarEquality(UnitRuleElimination, g, eG)
  }

  /** @see Hopcroft et al.'s Introduction to Automata Theory, Languages, and Computation, pp. 269, 271 <br>
    *      Liberty was taken to add a non-recursive start production delegating to the original start symbol 'I */
  it should "transform a simple arithmetic expression grammar correctly" in {
    val g = Grammar(
      'S := 'I,
      'I := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1",
      'F := 'I | "(" ~ 'E ~ ")",
      'T := 'F | 'T ~ "*" ~ 'F,
      'E := 'T | 'E ~ "+" ~ 'T,
    )
    val eG = Grammar(
      'S := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1",
      'I := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1",
      'F := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1" | "(" ~ 'E ~ ")",
      'T := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1" | "(" ~ 'E ~ ")" | 'T ~ "*" ~ 'F,
      'E := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1" | "(" ~ 'E ~ ")" | 'E ~ "+" ~ 'T | 'T ~ "*" ~ 'F,
    )
    checkGrammarEquality(UnitRuleElimination, g, eG)
  }

}
