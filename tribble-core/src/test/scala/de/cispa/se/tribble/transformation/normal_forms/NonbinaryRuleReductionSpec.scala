package de.cispa.se.tribble
package transformation
package normal_forms

import de.cispa.se.tribble.dsl._

class NonbinaryRuleReductionSpec extends TransformationTestSpecification {

  "Nonbinary rule reduction" should "ignore solitary terminals" in {
    val g = Grammar(
      'S := 'A | 'B,
      'A := "a1",
      'B := "[1-9]".regex
    )
    val eG = g
    checkGrammarEquality(NonbinaryRuleReduction, g, eG)
  }

  it should "ignore chain rules" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := "b1"
    )
    val eG = g
    checkGrammarEquality(NonbinaryRuleReduction, g, eG)
  }

  it should "cascade nonbinary concatenations" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B ~ 'C ~ 'D ~ 'E,
      'B := "b1",
      'C := "c1",
      'D := "d1",
      'E := "e1"
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'B ~ 'A_bin0,
      'A_bin0 := 'C ~ 'A_bin0_bin0,
      'A_bin0_bin0 := 'D ~ 'E,
      'B := "b1",
      'C := "c1",
      'D := "d1",
      'E := "e1"
    )
    checkGrammarEquality(NonbinaryRuleReduction, g, eG)
  }

  it should "cascade multiple concatenations in top-level alternations" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B ~ 'C ~ 'D | 'E ~ 'F ~ 'G,
      'B := "b1",
      'C := "c1",
      'D := "d1",
      'E := "e1",
      'F := "f1",
      'G := "g1",
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'B ~ 'A_bin0 | 'E ~ 'A_bin1,
      'A_bin0 := 'C ~ 'D,
      'A_bin1 := 'F ~ 'G,
      'B := "b1",
      'C := "c1",
      'D := "d1",
      'E := "e1",
      'F := "f1",
      'G := "g1",
    )
    checkGrammarEquality(NonbinaryRuleReduction, g, eG)
  }

}
