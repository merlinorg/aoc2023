package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.mutable

object Day14 extends AoC:
  private def tilt(lines: Vector[String]): Vector[String] =
    val array = lines.toArray.map(_.toArray)
    for
      y  <- lines.indices
      x  <- lines.indices
      if array(y)(x) == 'O'
      yᛌ <- (y to 0 by -1).find(y2 => y2 == 0 || array(y2 - 1)(x) != '.').filter(_ < y)
    do
      array(yᛌ)(x) = 'O'
      array(y)(x) = '.'
    array.toVector.map(_.mkString)

  private def load(lines: Vector[String]): Long =
    lines.zipWithIndex.foldMap:
      case (line, y) => line.count(_ == 'O') * (lines.length - y)

  private def rotate(lines: Vector[String]): Vector[String] =
    lines.transpose.map(_.reverse.mkString)

  override def a(lines: Vector[String]): Long =
    load(tilt(lines))

  override def b(lines: Vector[String]): Long =
    val cache  = mutable.Map.empty[Vector[String], Int]
    val result = for
      (from, to) <- Iterator
                      .iterate(lines): l =>
                        (0 until 4).foldLeft(l)((l, _) => rotate(tilt(l)))
                      .zipWithIndex
                      .map: (l, i) =>
                        i -> cache.getOrElseUpdate(l, i)
                      .find(_ > _)
      lastIndex   = to + (1000000000 - from) % (from - to)
      last        = cache.map(_.swap)(lastIndex)
    yield load(last)
    result | 0

end Day14
