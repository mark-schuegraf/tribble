package de.cispa.se.tribble
package transformation
package grammar_adaptation

import de.cispa.se.tribble.dsl._

class NonrecursiveRuleInliningSpec extends TransformationTestSpecification {

  "Nonrecursive rule inlining" should "inline nonrecursive references as usual" in {
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

  it should "not inline self-references" in {
    val g = Grammar(
      'S := 'A,
      'A := "" | "a" ~ 'A
    )
    val eG = g
    checkGrammarEquality(new NonrecursiveRuleInlining(1),g,eG)
  }

  it should "not inline mutual references" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B,
      'B := 'A | "a1",
    )
    val eG = g
    checkGrammarEquality(new NonrecursiveRuleInlining(1),g,eG)
  }

  it should "inline duplicate references" in {
    val g = Grammar(
      'S := 'A,
      'A := 'B ~ 'B,
      'B := "b1"
    )
    val eG = Grammar(
      'S := "b1" ~ "b1"
    )
    checkGrammarEquality(new NonrecursiveRuleInlining(2),g,eG)
  }

}
