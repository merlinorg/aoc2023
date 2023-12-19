package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day19 extends AoC:
  private type Rating = 'a' | 'x' | 's' | 'm'

  private type WorkflowName = String
  private final val Accept = "A"
  private final val Reject = "R"
  private final val In     = "in"

  private type Part  = Map[Rating, Long]
  private type Parts = Map[Rating, NumericRange[Long]]

  extension (self: Char) private def toRating: Rating = self match { case rating: Rating => rating }

  // a rule defines to which workflow a part goes, optionally based on a condition

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
              case s"$rating<$value:$dest" => ConditionalRule(rating.head.toRating, true, value.toLong, dest)
              case s"$rating>$value:$dest" => ConditionalRule(rating.head.toRating, false, value.toLong, dest)
              case dest                    => TerminalRule(dest),
        _.map:
          case s"{$part}" =>
            part.commaSeparated.mapToMap:
              case s"$rating=$value" => rating.head.toRating -> value.toLong
      )

  // divides parts according to a rule into which parts go to a new workflow and which continue processing

  private def routeParts(parts: Parts, rule: Rule): (Parts, (WorkflowName, Parts)) = rule match
    case TerminalRule(next)                         => Map.empty -> (next -> parts)
    case ConditionalRule(rating, less, limit, next) =>
      val (range, remaining) = if (less) parts(rating).splitLess(limit) else parts(rating).splitGreater(limit)
      parts.updated(rating, remaining) -> (next -> parts.updated(rating, range))

  // part 1, just thread all the parts through the workflows until they reach A

  override def part1(lines: Vector[String]): Long =
    val (workflows, parts) = lines.parse

    @tailrec def loop(name: WorkflowName, part: Part): Long = name match
      case Accept   => part.values.sum
      case Reject   => 0
      case workflow => loop(workflows(workflow).find(_.matches(part)).get.next, part)

    parts.foldMap(loop(In, _))

  // part 2, push all the part ranges through the workflows, splitting as necessary, until they reach A

  override def part2(lines: Vector[String]): Long =
    val (workflows, _) = lines.parse

    def loop(name: WorkflowName, parts: Parts): Long = name match
      case Accept   => parts.values.map(_.range).product
      case Reject   => 0
      case workflow => workflows(workflow).mapS(parts)(routeParts).foldMap(loop)

    loop(In, "amxs".toVector.mapToMap(_.toRating -> (1L until 4001L)))
