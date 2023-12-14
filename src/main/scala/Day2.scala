package org.merlin.aoc2023

import scalaz.Monoid
import scalaz.std.list.*
import scalaz.std.map.*
import scalaz.syntax.foldable.*

object Day2 extends AoC:
  private type Colours = Map[String, Int]

  private def isPlausible(round: Colours): Boolean =
    round("red") <= 12 && round("green") <= 13 && round("blue") <= 14

  private def parse(line: String): (Int, List[Colours]) = line match
    case s"Game $id: $results" =>
      val rounds =
        results
          .split("; ")
          .toList
          .map: round =>
            round
              .split(", ")
              .collect:
                case s"$count $colour" =>
                  colour -> count.toInt
              .toMap
              .withDefaultValue(0)
      id.toInt -> rounds

  override def part1(lines: Vector[String]): Long =
    val possibilities = lines
      .map(parse)
      .collect:
        case (id, rounds) if rounds.forall(isPlausible) => id

    possibilities.sum
  end part1

  override def part2(lines: Vector[String]): Long =
    implicit val maxMonoid: Monoid[Int] = Monoid.instance(_ max _, 0)

    val powers = lines
      .map(parse)
      .map:
        case (id, rounds) =>
          val minima = rounds.suml
          minima("red") * minima("green") * minima("blue")

    powers.sum
  end part2

end Day2
