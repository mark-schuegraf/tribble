package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

/** Includes variants of the test cases in [[GreibachFormalizerSpec]] that involve some unit rules instead of only nonterminal concatenations. */
class ExtendedGreibachFormalizerSpec extends TransformationTestSpecification {

  "Extended Greibach formalizer" should "substitute leftmost references and then back substitute leading references" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1",
      'B := "b1" | 'A
    )
    val eG = Grammar(
      'S := "a1" ~ 'B,
      'B := "b1" | "a1"
    )
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  it should "resolve self loops and then back substitute leading references" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1" | 'A,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "a1" ~ 'B,
      'B := "b1"
    )
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  it should "preserve unit rules when back substituting leading references" in {
    val g = Grammar(
      'S := 'A ~ 'A,
      'A := 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "b1" ~ 'A,
      'A := 'B,
      'B := "b1"
    )
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  it should "back substitute on grammars that have already been linearized" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | 'B ~ 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1" | "b1" ~ 'B,
      'B := "b1"
    )
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  it should "deal with inner left recursion, leaving helper unit rules intact" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | 'A ~ 'A
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1" | "a1" ~ 'A_lrec,
      'A_lrec := 'A | "a1" ~ 'A_lrec | "a1" ~ 'A_lrec ~ 'A_lrec
    )
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  it should "preserve start deletion rules" in {
    val g = Grammar(
      'S := "" | "a1" ~ 'A,
      'A := "a1"
    )
    val eG = g
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  it should "not create useless symbols" in {
    val g = Grammar(
      'S := 'A ~ "s1",
      'A := 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "b1" ~ "s1"
    )
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

  /** Integration test. Includes expected result grammars of constituent transformers for readability, though unused.
    *
    * @see Harrison's Introduction to Formal Language Theory, pp. 113, 114 <br>
    *      Liberty was taken to add a non-recursive start production delegating to the original start symbol 'A.
    *      The S', 'B and 'C productions were modified to include the two unit rule corner cases. */
  it should "transform a simple binary sequence grammar correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B ~ 'C,
      'B := "1" | 'B | 'A ~ 'B ~ 'C,
      'C := "0" | 'A
    )
    /* Step 1: linearization */
    val gLin = Grammar(
      'S := 'A,
      'A := 'B ~ 'C,
      'B := "1" | "1" ~ 'B_lrec,
      'B_lrec := 'C ~ 'B ~ 'C | 'C ~ 'B ~ 'C ~ 'B_lrec,
      'C := "0" | "1" ~ 'C |  "1" ~ 'B_lrec ~ 'C
    )
    /* Step 2: substitution (and removal of useless symbol 'A) */
    val gSub = Grammar(
      'S := 'A,
      'A := "1" ~ 'C | "1" ~ 'B_lrec ~ 'C,
      'B := "1" | "1" ~ 'B_lrec,
      'B_lrec := "0" ~ 'B ~ 'C | "0" ~ 'B ~ 'C ~ 'B_lrec | "1" ~ 'C ~ 'B ~ 'C | "1" ~ 'C ~ 'B ~ 'C ~ 'B_lrec | "1" ~ 'B_lrec ~ 'C ~ 'B ~ 'C | "1" ~ 'B_lrec ~ 'C ~ 'B ~ 'C ~ 'B_lrec,
      'C := "0" | "1" ~ 'C |  "1" ~ 'B_lrec ~ 'C
    )
    val eG = gSub
    checkGrammarEquality(ExtendedGreibachFormalizer, g, eG)
  }

}
