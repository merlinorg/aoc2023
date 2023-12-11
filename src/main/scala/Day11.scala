package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day11 extends AoC:
  type Point = (Long, Long)

  private def xExpansions(
    lines: Vector[String],
    expansion: Long
  ): IndexedSeq[Long] =
    (0 until lines.head.length).scanLeft(0L):
      case (offset, x) =>
        if (lines.forall(_.charAt(x) == '.')) expansion + offset else offset

  private def yExpansions(
    lines: Vector[String],
    expansion: Long
  ): IndexedSeq[Long] =
    lines.scanLeft(0L):
      case (offset, line) =>
        if (!line.contains('#')) expansion + offset else offset

  private def allStars(lines: Vector[String], expansion: Long): Vector[Point] =
    val xExpanse = xExpansions(lines, expansion)
    val yExpanse = yExpansions(lines, expansion)

    for
      (line, y) <- lines.zipWithIndex
      (chr, x)  <- line.zipWithIndex
      if chr == '#'
    yield (x.toLong + xExpanse(x), y.toLong + yExpanse(y))

  private def allPairs[A](list: List[A]): List[(A, A)] = list match
    case Nil          => Nil
    case head :: tail =>
      tail.strengthL(head) ::: allPairs(tail)

  private def sumDistances(stars: List[(Point, Point)]): Long =
    stars.foldMap:
      case ((x0, y0), (x1, y1)) =>
        (x0 - x1).abs + (y0 - y1).abs

  override def a(lines: Vector[String]): Long =
    sumDistances(allPairs(allStars(lines, 1).toList))
  end a

  override def b(lines: Vector[String]): Long =
    sumDistances(allPairs(allStars(lines, 999999).toList))
  end b

end Day11
