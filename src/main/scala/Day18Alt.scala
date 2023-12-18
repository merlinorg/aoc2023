package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day18Alt extends AoC:
  private def parse1(lines: Vector[String]): Vector[Vec] =
    lines.map { case s"$dir $count (#$_)" => Dir.byName(dir) * count.toInt }

  private def dirByNum(n: Int): Dir = Dir.fromOrdinal((n + 1) % 4)

  private def parse2(lines: Vector[String]): Vector[Vec] =
    lines.map { case s"$_ $_ (#$color)" => dirByNum(color.takeRight(1).toInt) * Integer.parseInt(color.take(5), 16) }

  private def area(vertices: Vector[Loc]): Long = // shoelace algorithm
    vertices.zip(vertices.tail).map((a, b) => a.x * b.y - b.x * a.y).sum.abs / 2

  private def outsideVertices(path: Vector[Vec]): Vector[Loc] =
    path
      .zip(path.tail :+ path.head)
      .scanLeft(false -> Origin): // safe initial state for a 0 0 start going east
        case ((priorWinding, location), (step, nextStep)) =>
          val winding = nextStep.direction == step.direction.cw // coming up to a right turn
          val edge    = if (winding != priorWinding) step else if (winding) step + 1 else step - 1
          winding -> (location + edge)
      .map(_._2)

  override def part1(lines: Vector[String]): Long = lines |> parse1 |> outsideVertices |> area

  override def part2(lines: Vector[String]): Long = lines |> parse2 |> outsideVertices |> area
