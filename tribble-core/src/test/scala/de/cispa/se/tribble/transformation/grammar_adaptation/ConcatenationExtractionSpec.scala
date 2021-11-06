package de.cispa.se.tribble
package transformation
package grammar_adaptation

import de.cispa.se.tribble.dsl._

class ConcatenationExtractionSpec extends TransformationTestSpecification {

  "The concatenation extraction" should "extract concatenations" in {
    val g = Grammar(
      /* used direct invocations of constructors to prevent flattening of
      'S := 'A | 'B,
      'A := "a1" ~ ('D ~ "a2").? ~ ("a3" ~ ('D ~ "a4" ~ "a5")).rep
      'B := "b1" ~ "b2" ~ ("b3" ~ ('D ~ 'C ~ "b4") ~ 'C)
      'C := "c1" ~ "c2" ~ ("c3" ~ "c4")
      'D := "d1" ~ "d2" */
      'S := 'A | 'B,
      'A := Concatenation(Seq("a1",
        Quantification(Concatenation(Seq('D, "a2")), 0, 1),
        Quantification(Concatenation(Seq("a3", Concatenation(Seq('D, "a4", "a5")))), 0, Int.MaxValue))),
      'B := Concatenation(Seq("b1", "b2",
        Concatenation(Seq("b3", Concatenation(Seq('D, 'C, "b4")), 'C)))),
      'C := Concatenation(Seq("c1", "c2",
        Concatenation(Seq("c3", "c4")))),
      'D := "d1" ~ "d2"
    )
    val eG = Grammar(
      'S := 'A | 'B,
      'A := "a1" ~ 'A_c0.? ~ 'A_c1.rep,
      'A_c0 := 'D ~ "a2",
      'A_c1 := "a3" ~ 'A_c1_c0,
      'A_c1_c0 := 'D ~ "a4" ~ "a5",
      'B := "b1" ~ "b2" ~ 'B_c0,
      'B_c0 := "b3" ~ 'B_c0_c0 ~ 'C,
      'B_c0_c0 := 'D ~ 'C ~ "b4",
      'C := "c1" ~ "c2" ~ 'C_c0,
      'C_c0 := "c3" ~ "c4",
      'D := "d1" ~ "d2"
    )
    checkGrammarEquality(ConcatenationExtraction, g, eG)
  }

  it should "deal with self-loops" in {
    val g = Grammar(
      'S := 'A,
      /* used direct invocation of constructors to prevent flattening of
      'A := "a1" | "a2" ~ ("a3" ~ 'A) */
      'A := Alternation(Seq("a1", Concatenation(Seq("a2", Concatenation(Seq("a3", 'A))))))
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1" | "a2" ~ 'A_c0,
      'A_c0 := "a3" ~ 'A
    )
    checkGrammarEquality(ConcatenationExtraction, g, eG)
  }

  it should "extract nested concatenations" in {
    val g = Grammar(
      'S := 'A,
      /* used direct invocation of constructors to prevent flattening of
      'A := "a1" ~ ( "a2" ~ ("a3" ~ 'B)))
      'B := "b1" */
      'A := Concatenation(Seq("a1", Concatenation(Seq("a2", Concatenation(Seq("a3", 'B)))))),
      'B := "b1"
    )
    val eG = Grammar(
      'S := 'A,
      'A := "a1" ~ 'A_c0,
      'A_c0 := "a2" ~ 'A_c0_c0,
      'A_c0_c0 := "a3" ~ 'B,
      'B := "b1"
    )
    checkGrammarEquality(ConcatenationExtraction, g, eG)
  }

}
