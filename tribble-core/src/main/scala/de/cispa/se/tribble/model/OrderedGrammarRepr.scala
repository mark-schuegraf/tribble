package de.cispa.se.tribble
package model

import de.cispa.se.tribble.generation.Reachability
import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.traverse.BreadthFirstIterator

import scala.collection.mutable
import collection.JavaConverters._

/** [[GrammarRepr]] enriched by an ordering over its rules. */
final case class OrderedGrammarRepr(grammar: GrammarRepr) {

  private val grammarGraph: Graph[DerivationRule, DefaultEdge] = Reachability.constructGraph(grammar)

  /** Orders the derivation rules of a grammar using the topological ordering of its grammar graph. */
  val orderedRules: IndexedSeq[DerivationRule] = {
    val buf = mutable.ListBuffer[DerivationRule]()
    val it = new BreadthFirstIterator(grammarGraph, grammar.root).asScala
    val noInternalNodesIt = it filter (grammar.rules.values.toSet contains _)
    buf appendAll noInternalNodesIt
    buf.toVector
  }

  /** Orders the productions of a grammar using the topological ordering of its grammar graph. */
  val orderedProductions: IndexedSeq[(NonTerminal, DerivationRule)] = {
    val orderedNonTerms: IndexedSeq[NonTerminal] = orderedRules map { rule =>
      val nonTermSearchResult = grammar.rules find { case (_, v) => v == rule }
      nonTermSearchResult.get._1
    }
    orderedNonTerms zip orderedRules
  }

  /** Provides index lookup functionality for nonterminals. */
  val orderedReferences: Map[NonTerminal, Int] = {
    val m = mutable.Map[NonTerminal, Int]()
    for ((rule, i) <- orderedRules.zipWithIndex) {
      val nonTermSearchResult = grammar.rules find { case (_, v) => v == rule }
      val nonTerm = nonTermSearchResult.get._1
      m(nonTerm) = i
    }
    m.toMap
  }

  def root: DerivationRule = grammar.root
  def apply(index: Int): DerivationRule = get(index)
  def apply(nonTerminal: NonTerminal): Int = get(nonTerminal)
  def apply(reference: Reference): Int = get(reference.name)
  def get(index: Int): DerivationRule = orderedRules(index)
  def get(nonTerminal: NonTerminal): Int = orderedReferences.getOrElse(nonTerminal,
    throw new NoSuchElementException(s"The grammar has no rule named $nonTerminal"))

}
