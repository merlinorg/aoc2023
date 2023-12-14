package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day9 extends AoC:

  @tailrec private def loop(v: List[Long], sum: Long): Long =
    v.zip(v.tail).map(_ - _) match
      case Nil   =>
        v.head + sum
      case diffs =>
        loop(diffs, v.head + sum)

  override def part1(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toList.reverse
      .foldMap: values =>
        loop(values, 0)
  end part1

  override def part2(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toList
      .foldMap: values =>
        loop(values, 0)
  end part2

end Day9
