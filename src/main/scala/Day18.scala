package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day18 extends AoC:
  extension (self: Dir.type) def byNum(i: Int): Dir = Dir.fromOrdinal((i + 1) % 4)

  private def parse1(lines: Vector[String]): Vector[Vec] =
    lines.map { case s"$dir $count (#$_)" => Dir.byName(dir) * count.toInt }

  private def parse2(lines: Vector[String]): Vector[Vec] =
    lines.map { case s"$_ $_ (#$color)" => Dir.byNum(color.takeRight(1).toInt) * Integer.parseInt(color.take(5), 16) }

  private def area(vertices: Vector[Loc]): Long = // shoelace algorithm
    vertices.zip(vertices.tail).map((a, b) => a.x * b.y - b.x * a.y).sum.abs / 2

  private def cellArea(path: Vector[Vec]): Long = // area of cell coordinates + perimeter / 2 + 1
    area(path.scanLeft(Origin)(_ + _)) + path.foldMap(_.magnitude) / 2 + 1

  override def part1(lines: Vector[String]): Long = lines |> parse1 |> cellArea

  override def part2(lines: Vector[String]): Long = lines |> parse2 |> cellArea
