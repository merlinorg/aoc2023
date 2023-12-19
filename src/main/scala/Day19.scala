package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.collection.mutable

object Day19 extends AoC:
  private sealed trait Rule:
    def next: String
    def matches(values: Map[String, Long]): Boolean

  private final case class TerminalRule(next: String) extends Rule:
    def matches(values: Map[String, Long]): Boolean = true

  private final case class ConditionalRule(rating: String, less: Boolean, value: Long, next: String) extends Rule:
    def matches(values: Map[String, Long]): Boolean = if (less) values(rating) < value else values(rating) > value

  extension (self: Vector[String])
    private def parse: (Map[String, Vector[Rule]], Vector[Map[String, Long]]) =
      self.split.bimap(
        _.map:
          case s"$name{$ruleSet}" =>
            name -> ruleSet.commaSeparated.map:
              case s"$prop<$value:$dest" => ConditionalRule(prop, true, value.toLong, dest)
              case s"$prop>$value:$dest" => ConditionalRule(prop, false, value.toLong, dest)
              case dest                  => TerminalRule(dest)
        .toMap,
        _.map:
          case s"{$part}" =>
            part.commaSeparated
              .map:
                case s"$name=$value" => name -> value.toLong
              .toMap
      )

  override def part1(lines: Vector[String]): Long =
    val (ruleSets, parts) = lines.parse

    @tailrec def loop(name: String, part: Map[String, Long]): Long =
      if (name == "A") part.values.sum
      else if (name == "R") 0
      else loop(ruleSets(name).find(_.matches(part)).get.next, part)

    parts.foldMap(loop("in", _))

  override def part2(lines: Vector[String]): Long =
    val (ruleSets, _) = lines.parse

    val parts0 = "amxs".split("").toList.strengthR[NumericRange[Long]](1L until 4001L).toMap
    val queue  = mutable.Queue("in" -> parts0)
    var total  = 0L

    while (queue.nonEmpty)
      val (name, parts) = queue.dequeue()
      val quantity      = parts.values.map(_.length.toLong).product
      if (name == "A") total = total + quantity
      else if (name != "R" && quantity > 0)
        ruleSets(name).scanLeft(parts):
          case (parts, TerminalRule(next))                         =>
            queue.enqueue(next -> parts)
            parts
          case (parts, ConditionalRule(rating, less, limit, next)) =>
            val range                 = parts(rating)
            val (subRange, remaining) =
              if (less) range.splitAt((limit - range.head).toInt)
              else range.splitAt((1 + limit - range.head).toInt).swap
            queue.enqueue(next -> parts.updated(rating, subRange))
            parts.updated(rating, remaining)
    total
