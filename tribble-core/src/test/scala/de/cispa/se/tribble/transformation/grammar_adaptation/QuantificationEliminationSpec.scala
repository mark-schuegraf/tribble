package de.cispa.se.tribble
package transformation
package grammar_adaptation

import de.cispa.se.tribble.dsl._

class QuantificationEliminationSpec extends TransformationTestSpecification {

  "Quantification elimination" should "eliminate optional quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".?
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0,
      'A_qe0 := "" | "a1"
    )
    checkGrammarEquality(QuantificationElimination, g, eG)
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
    checkGrammarEquality(QuantificationElimination, g, eG)
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
    checkGrammarEquality(QuantificationElimination, g, eG)
  }

  it should "turn singular quantifications into literal rules" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep(1, 1)
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0,
      'A_qe0 := "a1"
    )
    checkGrammarEquality(QuantificationElimination, g, eG)
  }

  it should "eliminate nested quantifications by adding new references delegating to their respective alternations" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep ~ ("a2".rep | 'A).rep(1)
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qe0 ~ 'A_qe1,
      'A_qe0 := "" | "a1" ~ 'A_qe0,
      /* direct invocation of Alternation() and Concatenation() to prevent flattening of
      'A_qe1 := ('A_qe1_qe0 | 'A) | ('A_qe1_qe0 | 'A) ~ 'A_qe1 */
      'A_qe1 := Alternation(Seq(Alternation(Seq('A_qe1_qe0, 'A)), Concatenation(Seq(Alternation(Seq('A_qe1_qe0, 'A)), 'A_qe1)))),
      'A_qe1_qe0 := "" | "a2" ~ 'A_qe1_qe0
    )
    checkGrammarEquality(QuantificationElimination, g, eG)
  }

  "Quantification elimination" should "fall back to quantification expansion if max is not infinity" in {
    val g = Grammar(
      'S := 'A | 'B,
      'A := "a1".rep(3, 3),
      'B := "b1".rep(4, 5)
    )
    val eG = Grammar(
      'S := 'A | 'B,
      'A := 'A_qe0,
      'A_qe0 := "a1" ~ "a1" ~ "a1",
      'B := 'B_qe0,
      'B_qe0 := "b1" ~ "b1" ~ "b1" ~ "b1" | "b1" ~ "b1" ~ "b1" ~ "b1" ~ "b1"
    )
    checkGrammarEquality(QuantificationElimination, g, eG)
  }

}
