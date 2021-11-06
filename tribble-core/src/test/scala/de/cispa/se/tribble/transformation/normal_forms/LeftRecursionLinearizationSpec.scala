package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

class LeftRecursionLinearizationSpec extends TransformationTestSpecification {

  "Left recursion linearization" should "substitute leftmost references correctly" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1",
      'B := "b1" | 'A ~ 'B
    )
    val eG = Grammar(
      'S := 'A ~ 'B,
      'A := "a1",
      'B := "b1" | "a1" ~ 'B
    )
    checkGrammarEquality(LeftRecursionLinearization, g, eG)
  }

  it should "resolve self loops correctly" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1" | 'A ~ 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := 'A ~ 'B,
      'A := "a1" | "a1" ~ 'A_lrec,
      'A_lrec := 'B | 'B ~ 'A_lrec,
      'B := "b1"
    )
    checkGrammarEquality(LeftRecursionLinearization, g, eG)
  }

  it should "not modify grammars with no left recursion" in {
    val g = Grammar(
      'S := 'A ~ 'B,
      'A := "a1" | 'B ~ 'B,
      'B := "b1"
    )
    val eG = g
    checkGrammarEquality(LeftRecursionLinearization, g, eG)
  }

  it should "deal with inner left recursion" in {
    val g = Grammar(
      'S := 'A ~ 'A,
      'A := "a1" | 'A ~ 'A
    )
    val eG = Grammar(
      'S := 'A ~ 'A,
      'A := "a1" | "a1" ~ 'A_lrec,
      'A_lrec := 'A | 'A ~ 'A_lrec
    )
    checkGrammarEquality(LeftRecursionLinearization, g, eG)
  }

  it should "preserve start deletion rules" in {
    val g = Grammar(
      'S := "" | 'A ~ 'A,
      'A := "a1"
    )
    val eG = g
    checkGrammarEquality(LeftRecursionLinearization, g, eG)
  }

  /** @see Harrison's Introduction to Formal Language Theory, pp. 113, 114 <br>
    *      Liberty was taken to add a non-recursive start production delegating to the original start symbol 'A */
  it should "linearize left recursion" in {
    val g = Grammar(
      'S := 'A ~ 'A,
      'A := 'B ~ 'C,
      'B := "1" | 'A ~ 'B,
      'C := "0" | 'A ~ 'C
    )
    val eG = Grammar(
      'S := 'A ~ 'A,
      'A := 'B ~ 'C,
      'B := "1" | "1" ~ 'B_lrec,
      'B_lrec := 'C ~ 'B | 'C ~ 'B ~ 'B_lrec,
      'C := "0" | "1" ~ 'C ~ 'C | "1" ~ 'B_lrec ~ 'C ~ 'C
    )
    checkGrammarEquality(LeftRecursionLinearization, g, eG)
  }

}
