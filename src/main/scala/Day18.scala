package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day18 extends AoC:
  private val directions1 = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N)
  private val directions2 = Map("0" -> Dir.E, "1" -> Dir.S, "2" -> Dir.W, "3" -> Dir.N)

  private def parse1(lines: Vector[String]): Vector[Vec] =
    lines.map:
      case s"$dir $count (#$_)" =>
        directions1(dir) * count.toInt

  private def parse2(lines: Vector[String]): Vector[Vec] =
    lines.map:
      case s"$_ $_ (#$color)" =>
        directions2(color.takeRight(1)) * Integer.parseInt(color.take(5), 16)

  private def area(locations: Vector[Loc]): Long =
    locations.zip(locations.tail).map((a, b) => a.x.toLong * b.y - b.x.toLong * a.y).sum.abs / 2

  private def perimeter(path: Vector[Vec]): Vector[Loc] =
    path
      .zip(path.tail :+ path.head)
      .scanLeft(false -> Loc(0, 0)): // safe initial state for a 0 0 start going east
        case ((priorWinding, location), (step, nextStep)) =>
          val winding = nextStep.direction == step.direction.cw // coming up to a right turn
          val edge    = if (winding != priorWinding) step else if (winding) step + 1 else step - 1
          winding -> (location + edge)
      .map(_._2)

  override def part1(lines: Vector[String]): Long = lines |> parse1 |> perimeter |> area

  override def part2(lines: Vector[String]): Long = lines |> parse2 |> perimeter |> area
