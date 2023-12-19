package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day19 extends AoC:
  private type Rating = 'a' | 'x' | 's' | 'm'

  private type WorkflowName = String

  private type Part = Map[Rating, Long]

  extension (self: String) private def toRating: Rating = self.charAt(0) match { case rating: Rating => rating }

  // a rule defines which workflow a part goes to, optionally based on a condition

  private sealed trait Rule:
    def next: WorkflowName
    def matches(values: Part): Boolean

  private final case class TerminalRule(next: WorkflowName) extends Rule:
    def matches(values: Part): Boolean = true

  private final case class ConditionalRule(rating: Rating, less: Boolean, value: Long, next: WorkflowName) extends Rule:
    def matches(values: Part): Boolean = if (less) values(rating) < value else values(rating) > value

  // parse the rules and the parts

  extension (self: Vector[String])
    private def parse: (Map[WorkflowName, Vector[Rule]], Vector[Part]) =
      self.split.bimap(
        _.mapToMap:
          case s"$name{$workflow}" =>
            name -> workflow.commaSeparated.map:
              case s"$rating<$value:$dest" => ConditionalRule(rating.toRating, true, value.toLong, dest)
              case s"$rating>$value:$dest" => ConditionalRule(rating.toRating, false, value.toLong, dest)
              case dest                    => TerminalRule(dest),
        _.map:
          case s"{$part}" =>
            part.commaSeparated.mapToMap:
              case s"$rating=$value" => rating.toRating -> value.toLong
      )

  // part 1, just thread all the parts through the workflows until they reach A

  override def part1(lines: Vector[String]): Long =
    val (workflows, parts) = lines.parse

    @tailrec def loop(name: WorkflowName, part: Part): Long = workflows.get(name) match
      case Some(workflow)      => loop(workflow.find(_.matches(part)).get.next, part)
      case None if name == "A" => part.values.sum
      case None                => 0

    parts.foldMap(loop("in", _))

  // part 2, push all the part ranges through the workflows, splitting as necessary, until they reach A

  override def part2(lines: Vector[String]): Long =
    val (workflows, _) = lines.parse

    def loop(name: WorkflowName, parts: Map[Rating, NumericRange[Long]]): Long = workflows.get(name) match
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

    loop("in", "amxs".characters.mapToMap(_.toRating -> (1L until 4001L)))
