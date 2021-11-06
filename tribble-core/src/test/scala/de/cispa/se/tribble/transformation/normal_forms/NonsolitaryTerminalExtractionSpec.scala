package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

class NonsolitaryTerminalExtractionSpec extends TransformationTestSpecification {

  "Nonsolitary terminal extraction" should "ignore solitary terminals" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1"
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1"
    )
    checkGrammarEquality(NonsolitaryTerminalExtraction, g, eG)
  }

  it should "eliminate nonsolitary terminals in concatenations" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" ~ 'B,
      'B := 'C ~ "b1" ~ "b2" ~ 'D,
      'C := "c1",
      'D := "d1"
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_term0 ~ 'B,
      'A_term0 := "a1",
      'B := 'C ~ 'B_term0 ~ 'B_term1 ~ 'D,
      'B_term0 := "b1",
      'B_term1 := "b2",
      'C := "c1",
      'D := "d1"
    )
    checkGrammarEquality(NonsolitaryTerminalExtraction, g, eG)
  }

  it should "eliminate nonsolitary terminals in top-level alternations" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1" ~ 'B | 'C,
      'B := "b1",
      'C := "c1"
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_term0 ~ 'B | 'C,
      'A_term0 := "a1",
      'B := "b1",
      'C := "c1"
    )
    checkGrammarEquality(NonsolitaryTerminalExtraction, g, eG)
  }

}
