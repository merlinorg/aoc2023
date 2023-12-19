package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day19 extends AoC:
  private type Part = Map[String, Long]

  // a rule defines which workflow a part goes to, optionally based on a condition

  private sealed trait Rule:
    def next: String
    def matches(values: Part): Boolean

  private final case class TerminalRule(next: String) extends Rule:
    def matches(values: Part): Boolean = true

  private final case class ConditionalRule(rating: String, less: Boolean, value: Long, next: String) extends Rule:
    def matches(values: Part): Boolean = if (less) values(rating) < value else values(rating) > value

  // parse the rules and the parts

  extension (self: Vector[String])
    private def parse: (Map[String, Vector[Rule]], Vector[Part]) =
      self.split.bimap(
        _.mapToMap:
          case s"$name{$workflow}" =>
            name -> workflow.commaSeparated.map:
              case s"$prop<$value:$dest" => ConditionalRule(prop, true, value.toLong, dest)
              case s"$prop>$value:$dest" => ConditionalRule(prop, false, value.toLong, dest)
              case dest                  => TerminalRule(dest),
        _.map:
          case s"{$part}" =>
            part.commaSeparated.mapToMap:
              case s"$name=$value" => name -> value.toLong
      )

  // part 1, just thread all the parts through the workflows until they reach A

  override def part1(lines: Vector[String]): Long =
    val (workflows, parts) = lines.parse

    @tailrec def loop(name: String, part: Part): Long = workflows.get(name) match
      case Some(workflow)      => loop(workflow.find(_.matches(part)).get.next, part)
      case None if name == "A" => part.values.sum
      case None                => 0

    parts.foldMap(loop("in", _))

  // part 2, push all the part ranges through the workflows, splitting as necessary, until they reach A

  override def part2(lines: Vector[String]): Long =
    val (workflows, _) = lines.parse

    def loop(name: String, parts: Map[String, NumericRange[Long]]): Long = workflows.get(name) match
      // if we match a workflow then split the parts among each of the rules
      case Some(workflow) =>
        val (_, ruleMatches) = workflow.mapAccumL(parts):
          case (parts, TerminalRule(next))                         =>
            Map.empty -> (next -> parts)
          case (parts, ConditionalRule(rating, less, limit, next)) =>
            val (range, remaining) = if (less) parts(rating).splitLess(limit) else parts(rating).splitGreater(limit)
            parts.updated(rating, remaining) -> (next -> parts.updated(rating, range))
        ruleMatches.foldMap(loop)

      // on acceptance the number of parts is all possible combinations aka product of all the range sizes
      case None if name == "A" => parts.values.map(_.length.toLong).product

      case _ => 0

    loop("in", "amxs".characters.mapToMap(_ -> (1L until 4001L)))
