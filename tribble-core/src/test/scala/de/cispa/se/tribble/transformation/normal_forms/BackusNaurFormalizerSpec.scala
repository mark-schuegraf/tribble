package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

/** Tests whether a grammar is in BNF after formalization.
  * Perhaps later add tests for regex compliance. */
class BackusNaurFormalizerSpec extends TransformationTestSpecification {

  "Backus-Naur formalizer" should "eliminate internal alternations" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | "a2" ~ ("a3" | "a4")
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1" | "a2" ~ 'A_a0,
      'A_a0 := "a3" | "a4"
    )
    checkGrammarEquality(BackusNaurFormalizer, g, eG)
  }

  it should "eliminate optional quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".?
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0,
      'A_qe0 := "" | "a1"
    )
    checkGrammarEquality(BackusNaurFormalizer, g, eG)
  }

  it should "eliminate Kleene star quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0,
      'A_qe0 := "" | "a1" ~ 'A_qe0
    )
    checkGrammarEquality(BackusNaurFormalizer, g, eG)
  }

  it should "eliminate Kleene plus quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep(1)
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0,
      'A_qe0 := "a1" | "a1" ~ 'A_qe0
    )
    checkGrammarEquality(BackusNaurFormalizer, g, eG)
  }

  it should "eliminate quantifications nested within internal alternations correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" | "a2" ~ ("a3".rep | "a4".rep(1))
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1" | "a2" ~ 'A_a0,
      'A_a0 := 'A_a0_qe0 | 'A_a0_qe1,
      'A_a0_qe0 := "" | "a3" ~ 'A_a0_qe0,
      'A_a0_qe1 := "a4" | "a4" ~ 'A_a0_qe1

    )
    checkGrammarEquality(BackusNaurFormalizer, g, eG)
  }

  it should "eliminate internal alternations nested within quantifications correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := ("a3" | "a4").rep
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0,
      'A_qe0 := "" | 'A_a0 ~ 'A_qe0,
      'A_a0 := "a3" | "a4"
    )
    checkGrammarEquality(BackusNaurFormalizer, g, eG)
  }

}
