package org.merlin.aoc2023

import scala.annotation.tailrec

object Day9 extends AoC:

  private def getDiffs(v: Vector[Long]): Vector[Long] =
    v.sliding(2).map(pair => pair.last - pair.head).toVector

  @tailrec private def aLoop(v: Vector[Long], sum: Long): Long =
    val diffs = getDiffs(v)
    if (diffs.forall(_ == 0))
      v.last + sum
    else
      aLoop(diffs, v.last + sum)

  override def a(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toVector
      .map: values =>
        aLoop(values, 0)
      .sum
  end a

  private def bLoop(v: Vector[Long]): Long =
    val diffs = getDiffs(v)
    if (diffs.forall(_ == 0))
      v.head
    else
      v.head - bLoop(diffs)

  override def b(lines: Vector[String]): Long =
    lines
      .map: line =>
        NumRe.findAllIn(line).map(_.toLong).toVector
      .map: values =>
        bLoop(values)
      .sum
  end b

end Day9
