package de.cispa.se.tribble
package generation

import scala.collection.mutable
import scala.util.Random

private[tribble] class SizedTreeGenerator(maxRepetitions: Int, random: Random, shortestTreeGenerator: ShortestTreeGenerator, requestedSize: Int, heuristic: Heuristic) extends TreeGenerator {

  private def childSlots(node: DNode)(implicit grammar: GrammarRepr): Seq[Slot] = node.decl match {
      case r: Reference => Slot(grammar(r), 0, node) :: Nil
      case Concatenation(elements, _) => elements.zipWithIndex.map { case (e, i) => Slot(e, i, node) }
      case Alternation(alternatives, _) =>
        // try to choose the most slot-rich alternative
        val maxSlots = alternatives.maxBy(_.shortestDerivation).shortestDerivation
        val longest = alternatives.filter(_.shortestDerivation == maxSlots).toList
        Slot(longest(random.nextInt(longest.size)), 0, node) :: Nil
      case Quantification(subject, min, max, _) =>
        val lower = Math.max(1, min)
        val constrainedMax = Math.max(lower, Math.min(max, maxRepetitions))
        Stream.fill(lower + random.nextInt(1 + constrainedMax - lower))(subject).zipWithIndex.map { case (e, i) => Slot(e, i, node) }
      case _: TerminalRule => Nil
    }

  // Based on the PTC2 Algorithm by Sean Luke
  private[tribble] def gen(root: DNode)(implicit grammar: GrammarRepr): DTree = {
    heuristic.startedTree()
    val Q = mutable.ListBuffer[Slot]()
    var c = 1
    Q ++= childSlots(root)
    while (Q.nonEmpty && c + Q.size < requestedSize) {
      val Slot(rule, index, parent) = heuristic.pickNext(Q)
      c += 1
      val child = rule match {
        case t: TerminalRule => shortestTreeGenerator.gen(t, Some(parent), 0)
        case _ =>
          val ch = DNode(rule, Some(parent))
          Q ++= childSlots(ch)
          ch
      }
      parent.children(index) = child
      heuristic.createdNode(child)
    }
    for (Slot(rule, index, parent) <- Q) {
      val child = shortestTreeGenerator.gen(rule, Some(parent), 0) // XXX might want to use a lower-n-percentile-based tree generator here
      parent.children(index) = child
      // inform the heuristic about everything generated by the shortestTreeGenerator
      child dfs {n => heuristic.createdNode(n) }
    }
    heuristic.finishedTree(root)
    root
  }

  override private[tribble] def gen(decl: DerivationRule, parent: Option[DNode], currentDepth : Int)(implicit grammar: GrammarRepr): DTree = decl match {
    case t: TerminalRule => shortestTreeGenerator.gen(t, parent, currentDepth)
    case _ => gen(DNode(decl, parent))
  }
}
