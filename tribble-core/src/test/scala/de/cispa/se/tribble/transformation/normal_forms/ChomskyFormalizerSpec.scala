package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

class ChomskyFormalizerSpec extends TransformationTestSpecification {

  "Chomsky formalizer" should "eliminate terminal symbols except in rules of size 1 (TERM)" in {
    val g = Grammar(
      'S := "s1" ~ "s2",
    )
    val eG = Grammar(
      'S := 'S_term0 ~ 'S_term1,
      'S_term0 := "s1",
      'S_term1 := "s2"
    )
    checkGrammarEquality(ChomskyFormalizer, g, eG)
  }

  it should "reduce rules to size 2 (BIN)" in {
    val g = Grammar(
      'S := 'A ~ 'B ~ 'C,
      'A := "a1",
      'B := "b1",
      'C := "c1"
    )
    val eG = Grammar(
      'S := 'A ~ 'S_bin0,
      'S_bin0 := 'B ~ 'C,
      'A := "a1",
      'B := "b1",
      'C := "c1"
    )
    checkGrammarEquality(ChomskyFormalizer, g, eG)
  }

  it should "eliminate deletion rules (DEL)" in {
    val g = Grammar(
      'S := "s1" | 'A,
      'A := 'B ~ 'B,
      'B := "" | 'A ~ 'A
    )
    val eG = Grammar(
      'S := "" | "s1"
    )
    checkGrammarEquality(ChomskyFormalizer, g, eG)
  }

  it should "eliminate unit rules (UNIT)" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | 'B,
      'B := "b1" | 'A
    )
    val eG = Grammar(
      'S := "a1" | "b1"
    )
    checkGrammarEquality(ChomskyFormalizer, g, eG)
  }

  /** Integration test. Includes expected result grammars of constituent transformers for readability, though unused.
    *
    * @see Hopcroft et al.'s Introduction to Automata Theory, Languages, and Computation, pp. 269, 271 <br>
    *      Liberty was taken to add a non-recursive start production delegating to the original start symbol 'I.
    *      Additionally, an epsilon production was added to diversify the grammar. */
  it should "transform a simple arithmetic expression grammar correctly" in {
    val g = Grammar(
      'S := 'D | 'I,
      'D := "",
      'I := "a" | "b" | 'I ~ "a" | 'I ~ "b" | 'I ~ "0" | 'I ~ "1",
      'F := 'I | "(" ~ 'E ~ ")",
      'T := 'F | 'T ~ "*" ~ 'F,
      'E := 'T | 'E ~ "+" ~ 'T,
    )
    /* Step 1: term */
    val gTerm = Grammar(
      'S := 'D | 'I,
      'D := "",
      'I := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3,
      'I_term0 := "0",
      'I_term1 := "a",
      'I_term2 := "b",
      'I_term3 := "1",
      'F := 'I | 'F_term0 ~ 'E ~ 'F_term1,
      'F_term0 := "(",
      'F_term1 := ")",
      'T := 'F | 'T ~ 'T_term0 ~ 'F,
      'T_term0 := "*",
      'E := 'T | 'E ~ 'E_term0 ~ 'T,
      'E_term0 := "+"
    )
    /* Step 2: bin */
    val gBin = Grammar(
      'S := 'D | 'I,
      'D := "",
      'I := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3,
      'I_term0 := "0",
      'I_term1 := "a",
      'I_term2 := "b",
      'I_term3 := "1",
      'F := 'I | 'F_term0 ~ 'F_bin0,
      'F_bin0 := 'E ~ 'F_term1,
      'F_term0 := "(",
      'F_term1 := ")",
      'T := 'F | 'T ~ 'T_bin0,
      'T_bin0 := 'T_term0 ~ 'F,
      'T_term0 := "*",
      'E := 'T | 'E ~ 'E_bin0,
      'E_bin0 := 'E_term0 ~ 'T,
      'E_term0 := "+"
    )
    /* Step 3: del */
    val gDel = Grammar(
      'S := "" | 'I,
      'I := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3,
      'I_term0 := "0",
      'I_term1 := "a",
      'I_term2 := "b",
      'I_term3 := "1",
      'F := 'I | 'F_term0 ~ 'F_bin0,
      'F_bin0 := 'E ~ 'F_term1,
      'F_term0 := "(",
      'F_term1 := ")",
      'T := 'F | 'T ~ 'T_bin0,
      'T_bin0 := 'T_term0 ~ 'F,
      'T_term0 := "*",
      'E := 'T | 'E ~ 'E_bin0,
      'E_bin0 := 'E_term0 ~ 'T,
      'E_term0 := "+"
    )
    /* Step 4: unit */
    val gUnit = Grammar(
      'S := "" | "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3,
      'I := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3,
      'I_term0 := "0",
      'I_term1 := "a",
      'I_term2 := "b",
      'I_term3 := "1",
      'F := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3 | 'F_term0 ~ 'F_bin0,
      'F_bin0 := 'E ~ 'F_term1,
      'F_term0 := "(",
      'F_term1 := ")",
      'T := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3 | 'F_term0 ~ 'F_bin0 | 'T ~ 'T_bin0,
      'T_bin0 := 'T_term0 ~ 'F,
      'T_term0 := "*",
      'E := "a" | "b" | 'I ~ 'I_term0 | 'I ~ 'I_term1 | 'I ~ 'I_term2 | 'I ~ 'I_term3 | 'F_term0 ~ 'F_bin0 | 'T ~ 'T_bin0 | 'E ~ 'E_bin0,
      'E_bin0 := 'E_term0 ~ 'T,
      'E_term0 := "+"
    )
    val eG = gUnit
    checkGrammarEquality(ChomskyFormalizer, g, eG)
  }

}
