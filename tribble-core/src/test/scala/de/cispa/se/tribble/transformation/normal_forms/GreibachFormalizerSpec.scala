package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

/** Most test cases are continuations of the test cases in [[LeftRecursionLinearizationSpec]]. */
class GreibachFormalizerSpec extends TransformationTestSpecification {

  "Greibach formalizer" should "substitute leftmost references and then back substitute leading references" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1",
      'B := "b1" | 'A ~ 'B
    )
    val eG = Grammar(
      'S := "a1" ~ 'B,
      'B := "b1" | "a1" ~ 'B
    )
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

  it should "resolve self loops and then back substitute leading references" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1" | 'A ~ 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "a1" ~ 'B | "a1" ~ 'A_lrec ~ 'B,
      'A_lrec := "b1" | "b1" ~ 'A_lrec,
      'B := "b1"
    )
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

  it should "back substitute on grammars that have already been linearized" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1" | 'B ~ 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "a1" ~ 'B | "b1" ~ 'B ~ 'B,
      'B := "b1"
    )
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

  it should "deal with inner left recursion" in {
    val g = Grammar(
      'S := 'A ~ 'A,
      'A := "a1" | 'A ~ 'A
    )
    val eG = Grammar(
      'S := "a1" ~ 'A | "a1" ~ 'A_lrec ~ 'A,
      'A := "a1" | "a1" ~ 'A_lrec,
      'A_lrec := "a1" | "a1" ~ 'A_lrec | "a1" ~ 'A_lrec ~ 'A_lrec
    )
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

  it should "preserve start deletion rules" in {
    val g = Grammar(
      'S := "" | "a1" ~ 'A,
      'A := "a1"
    )
    val eG = g
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

  it should "not create useless symbols" in {
    val g = Grammar(
      'S := 'A ~ 'A,
      'A := "a1" | 'B ~ 'A,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "a1" ~ 'A | "b1" ~ 'A ~ 'A,
      'A := "a1" | "b1" ~ 'A
    )
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

  /** Integration test. Includes expected result grammars of constituent transformers for readability, though unused.
    *
    * @see Harrison's Introduction to Formal Language Theory, pp. 113, 114 <br>
    *      Liberty was taken to add a non-recursive start production delegating to the original start symbol 'A */
  it should "transform a simple binary sequence grammar correctly" in {
    val g = Grammar(
      'S := 'A ~ 'A,
      'A := 'B ~ 'C,
      'B := "1" | 'A ~ 'B,
      'C := "0" | 'A ~ 'C
    )
    /* Step 1: linearization */
    val gLin = Grammar(
      'S := 'A ~ 'A,
      'A := 'B ~ 'C,
      'B := "1" | "1" ~ 'B_lrec,
      'B_lrec := 'C ~ 'B | 'C ~ 'B ~ 'B_lrec,
      'C := "0" | "1" ~ 'C ~ 'C | "1" ~ 'B_lrec ~ 'C ~ 'C
    )
    /* Step 2: substitution */
    val gSub = Grammar(
      'S := "1" ~ 'C ~ 'A | "1" ~ 'B_lrec ~ 'C ~ 'A,
      'A := "1" ~ 'C | "1" ~ 'B_lrec ~ 'C,
      'B := "1" | "1" ~ 'B_lrec,
      'B_lrec := "0" ~ 'B | "1" ~ 'C ~ 'C ~ 'B | "1" ~ 'B_lrec ~ 'C ~ 'C ~ 'B | "0" ~ 'B ~ 'B_lrec | "1" ~ 'C ~ 'C ~ 'B ~ 'B_lrec | "1" ~ 'B_lrec ~ 'C ~ 'C ~ 'B ~ 'B_lrec,
      'C := "0" | "1" ~ 'C ~ 'C | "1" ~ 'B_lrec ~ 'C ~ 'C
    )
    val eG = gSub
    checkGrammarEquality(GreibachFormalizer, g, eG)
  }

}
