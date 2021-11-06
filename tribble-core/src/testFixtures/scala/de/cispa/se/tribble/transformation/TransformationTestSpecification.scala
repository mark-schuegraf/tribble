package de.cispa.se.tribble
package transformation

import de.cispa.se.tribble.dsl.Grammar
import de.cispa.se.tribble.input.{AssignIds, ModelAssembler, SharedModelAssembler}
import de.cispa.se.tribble.output.GrammarPrettyPrinter
import org.scalatest.AppendedClues.convertToClueful

abstract class TransformationTestSpecification extends TestSpecification with SharedModelAssembler {

  /** Transforms the original grammar under the given [[GrammarTransformer]]. Then asserts that it is equal to the expected grammar. <br>
    * Note: strips each [[GrammarRepr]] of its ids, since: <br>
    * 1. Grammar elements added by transformation will have larger ids than pre-inserted ones <br>
    * 2. The internal [[Map]] rule ordering may differ <br>
    * Due to the latter an id reassignment via [[AssignIds]] also leads to a different [[GrammarRepr]] and is thus not a post-processing option. <br>
    *
    * @param transformer     to transform the original grammar
    * @param originalGrammar to turn into a variant for comparison
    * @param expectedGrammar to compare the variant to
    * @param assembler       to override the shared model assembler with a specially configured one
    */
  def checkGrammarEquality(transformer: GrammarTransformer, originalGrammar: Grammar, expectedGrammar: Grammar, assembler: ModelAssembler = modelAssembler): Unit = {
    val original = assembler.assemble(originalGrammar.productions)
    val transformed = transformer.transformGrammar(original)
    val expected = assembler.assemble(expectedGrammar.productions)
    val transformedNoIds = ResetIds.process(transformed)
    val expectedNoIds = ResetIds.process(expected)
    transformedNoIds shouldEqual expectedNoIds withClue grammarDiff(transformedNoIds, expectedNoIds)
  }

  /** Compares two grammars and serializes their respective unidirectional differences. */
  def grammarDiff(transformed: GrammarRepr, expected: GrammarRepr): String = {
    def unidirectionalDiff(g1: GrammarRepr, g2: GrammarRepr): String = {
      val diffRules = (g1.rules.toSet diff g2.rules.toSet).toMap
      val diffGrammar = GrammarRepr(g1.start, diffRules)
      new GrammarPrettyPrinter(diffGrammar).prettyPrint()
    }

    val diffTransformed: String = unidirectionalDiff(transformed, expected)
    val diffExpected: String = unidirectionalDiff(expected, transformed)
    s"\nGrammar diff:\nTransformed ${diffTransformed}Expected $diffExpected\n"
  }

}
