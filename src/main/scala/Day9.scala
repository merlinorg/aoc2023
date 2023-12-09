package org.merlin.aoc2023

import scala.annotation.tailrec

object Day9 extends AoC:

  @tailrec private def loop(v: List[Long], sum: Long): Long =
    v.zip(v.tail).map(_ - _) match
      case Nil   =>
        v.head + sum
      case diffs =>
        loop(diffs, v.head + sum)

  override def a(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toList.reverse
      .map: values =>
        loop(values, 0)
      .sum
  end a

  override def b(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toList
      .map: values =>
        loop(values, 0)
      .sum
  end b

end Day9
