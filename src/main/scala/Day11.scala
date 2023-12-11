package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day11 extends AoC:
  type Point = (Long, Long)

  extension (pt: Point)
    def Δ(op: Point): Long = (pt._1 - op._1).abs + (pt._2 - op._2).abs

  private def inflate(
    lines: Vector[Vector[Char]],
    factor: Long
  ): IndexedSeq[Long] =
    lines.scanLeft(0L): (offset, line) =>
      if (!line.contains('#')) offset + factor else offset + 1

  private def allStars(
    lines: Vector[String],
    factor: Long
  ): Vector[Point] =
    val xs = inflate(lines.transpose, factor)
    val ys = inflate(lines.map(_.toVector), factor)
    for
      (line, y) <- lines.zip(ys)
      (chr, x)  <- line.zip(xs)
      if chr == '#'
    yield x -> y

  override def a(lines: Vector[String]): Long =
    allStars(lines, 2).combinations(2).toList.foldMap(v => v(0) Δ v(1))
  end a

  override def b(lines: Vector[String]): Long =
    allStars(lines, 1000000).combinations(2).toList.foldMap(v => v(0) Δ v(1))
  end b

end Day11
