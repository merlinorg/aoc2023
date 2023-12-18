package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day18 extends AoC:
  private val directions1 = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N)
  private val directions2 = Map("0" -> Dir.E, "1" -> Dir.S, "2" -> Dir.W, "3" -> Dir.N)

  private def parse1(lines: Vector[String]): Vector[Vec] =
    lines.map { case s"$dir $count (#$_)" => directions1(dir) * count.toInt }

  private def parse2(lines: Vector[String]): Vector[Vec] =
    lines.map { case s"$_ $_ (#$color)" => directions2(color.takeRight(1)) * Integer.parseInt(color.take(5), 16) }

  private def area(vertices: Vector[Loc]): Long = // shoelace algorithm
    vertices.zip(vertices.tail).map((a, b) => a.x.toLong * b.y - b.x.toLong * a.y).sum.abs / 2

  private def cellArea(path: Vector[Vec]): Long = // area of cell coordinates + perimeter / 2 + 1
    area(path.scanLeft(origin)(_ + _)) + path.foldMap(_.length) / 2 + 1

  override def part1(lines: Vector[String]): Long = lines |> parse1 |> cellArea

  override def part2(lines: Vector[String]): Long = lines |> parse2 |> cellArea
