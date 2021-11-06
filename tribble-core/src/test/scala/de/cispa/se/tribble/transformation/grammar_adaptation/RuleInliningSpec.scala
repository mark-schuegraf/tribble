package de.cispa.se.tribble
package transformation
package grammar_adaptation

import de.cispa.se.tribble.dsl._

class RuleInliningSpec extends TransformationTestSpecification {

  "The RuleInlining" should "inline references correctly" in {
    val g = Grammar(
      'A := "a" ~ 'B ~ 'C,
      'B := "b" ~ 'C,
      'C := "c"
    )
    val grammar = modelAssembler.assemble(g.productions)
    val inlinedOnce = new RuleInlining(1).transformGrammar(grammar)
    inlinedOnce.rules should have size 2

    val inlinedTwice = new RuleInlining(2).transformGrammar(grammar)
    inlinedTwice.rules should have size 1
  }

  it should "be able to inline references multiple times" in {
    val g = Grammar(
      'S := 'A,
      'A := "" | "a" ~ 'A
    )
    val grammar = modelAssembler.assemble(g.productions)
    for (repetitions <- 1 to 7) {
      val inlining = new RuleInlining(repetitions)
      val inlined = inlining.transformGrammar(grammar)
      inlined("A").toStream.length shouldEqual math.pow(2, repetitions + 2).intValue + 1
    }
  }
}
