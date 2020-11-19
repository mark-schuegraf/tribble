package de.cispa.se.tribble.input

import de.cispa.se.tribble.{DerivationRule, GrammarRepr}

/** Resets the id of all derivation rules to [[DerivationRule.DEFAULT_ID]]. Useful only in testing. */
private[input] object ResetIds extends AssemblyPhase {
  override def process(grammar: GrammarRepr): GrammarRepr = {
    grammar.rules.values.flatMap(_.toStream).foreach(_.id = DerivationRule.DEFAULT_ID)
    grammar
  }
}
