package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day18 extends AoC:
  final val dirs  = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N)
  final val dirs2 = Map("0" -> Dir.E, "1" -> Dir.S, "2" -> Dir.W, "3" -> Dir.N)

  private def parse1(lines: Vector[String]): Vector[(Dir, Int)] =
    lines.map:
      case s"$dir $count (#$_)" =>
        dirs(dir) -> count.toInt

  private def parse2(lines: Vector[String]): Vector[(Dir, Int)] =
    lines.map:
      case s"$_ $_ (#$color)" =>
        dirs2(color.takeRight(1)) -> Integer.parseInt(color.take(5), 16)

  private def area(coords: Vector[Loc]): Long =
    coords.zip(coords.tail).map((a, b) => a.x.toLong * b.y - b.x.toLong * a.y).sum.abs / 2

  private def perimeter(rules: Vector[(Dir, Int)]): Vector[Loc] =
    rules
      .zip(rules.tail :+ rules.head)
      .scanLeft(false -> Loc(0, 0)): // not generally correct since I assume prior winding
        case ((occw, loc), ((dir, count), (ndir, _))) =>
          val ccw = dir == ndir.ccw
          ccw -> loc.mulAdd(dir, count + (if (ccw != occw) 0 else if (ccw) 1 else -1))
      .map(_._2)

  override def part1(lines: Vector[String]): Long = area(perimeter(parse1(lines)))

  override def part2(lines: Vector[String]): Long = area(perimeter(parse2(lines)))
