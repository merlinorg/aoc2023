package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day11 extends AoC:
  type Point = (Long, Long)

  private def locations(
    lines: Vector[Vector[Char]],
    expansion: Long
  ): IndexedSeq[Long] =
    lines.scanLeft(0L):
      case (offset, line) =>
        if (!line.contains('#')) offset + expansion else offset + 1

  private def allStars(
    lines: Vector[Vector[Char]],
    expansion: Long
  ): Vector[Point] =
    val yLocs = locations(lines, expansion)
    val xLocs = locations(lines.transpose, expansion)
    for
      (line, y) <- lines.zip(yLocs)
      (chr, x)  <- line.zip(xLocs)
      if chr == '#'
    yield x -> y

  private def sumDistances(stars: Iterator[Vector[Point]]): Long =
    stars.toList.foldMap:
      case Vector((x0, y0), (x1, y1)) =>
        (x0 - x1).abs + (y0 - y1).abs
      case _                          => 0

  override def a(lines: Vector[String]): Long =
    sumDistances(allStars(lines.map(_.toVector), 2).combinations(2))
  end a

  override def b(lines: Vector[String]): Long =
    sumDistances(allStars(lines.map(_.toVector), 1000000).combinations(2))
  end b

end Day11
