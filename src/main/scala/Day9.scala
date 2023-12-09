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

  override def a(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toList.reverse
      .foldMap: values =>
        loop(values, 0)
  end a

  override def b(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toList
      .foldMap: values =>
        loop(values, 0)
  end b

end Day9
