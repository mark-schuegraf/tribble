package de.cispa.se.tribble
package transformation
package grammar_adaptation

import de.cispa.se.tribble.dsl._

class QuantificationExpansionSpec extends TransformationTestSpecification {

  val quantificationExpansion: Seq[GrammarTransformer] = for (i <- Range(0, 4)) yield new QuantificationExpansion(i)

  "Quantification expansion" should "create references to the original quantification subjects when no repetitions are demanded" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0,
      'A_qx0 := "a1".rep
    )
    checkGrammarEquality(quantificationExpansion(0), g, eG)
  }

  it should "expand optional quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".?
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0,
      'A_qx0 := "" | "a1"
    )
    checkGrammarEquality(quantificationExpansion(2), g, eG)
  }

  it should "expand Kleene star quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0,
      'A_qx0 := "" | "a1" | "a1".rep(2)
    )
    checkGrammarEquality(quantificationExpansion(2), g, eG)
  }

  it should "expand Kleene plus quantifiers correctly" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep(1)
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0,
      'A_qx0 := "a1" | "a1" ~ "a1" | "a1".rep(3)
    )
    checkGrammarEquality(quantificationExpansion(2), g, eG)
  }

  it should "reduce dangling language preservation quantifiers to their subjects, if only one binding remains" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".?
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0,
      'A_qx0 := "" | "a1" // simplified form of "" | "a1".rep(1,1)
    )
    checkGrammarEquality(quantificationExpansion(1), g, eG)
  }

  it should "stop expansion when a quantification is empty, even if the repetition limit has not been reached" in {
    /* repetitionLimit = 3, but quantification "a1".? has only two alternatives to extract */
    val g = Grammar(
      'S := 'A,
      'A := "a1".?
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0,
      'A_qx0 := "" | "a1"
    )
    checkGrammarEquality(quantificationExpansion(3), g, eG)
  }

  it should "expand nested quantifications by adding new references delegating to their respective alternations" in {
    val g = Grammar(
      'S := 'A,
      'A := "a1".rep ~ ("a2".rep | 'A).rep(1)
    )
    val eG = Grammar(
      'S := 'A,
      'A := 'A_qx0 ~ 'A_qx1,
      'A_qx0 := "" | "a1" | "a1".rep(2),
      /* direct invocation of Alternation() to prevent flattening of
      'A_qx1 := ('A_qx1_qx0 | 'A) | ('A_qx1_qx0 | 'A) ~ ('A_qx1_qx0 | 'A) | ('A_qx1_qx0 | 'A).rep(3) */
      'A_qx1 := Alternation(Seq(Alternation(Seq('A_qx1_qx0, 'A)), ('A_qx1_qx0 | 'A) ~ ('A_qx1_qx0 | 'A), ('A_qx1_qx0 | 'A).rep(3))),
      'A_qx1_qx0 := "" | "a2" | "a2".rep(2)
    )
    checkGrammarEquality(quantificationExpansion(2), g, eG)
  }

}
