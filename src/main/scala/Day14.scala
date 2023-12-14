package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day14 extends AoC:
  private def tilt(lines: Vector[String]): Vector[String] =
    val array = lines.toArray.map(_.toArray)
    for
      y  <- lines.indices
      x  <- lines.indices
      if array(y)(x) == 'O'
      yᛌ <- (y to 0 by -1).find(yᛌ => yᛌ == 0 || array(yᛌ - 1)(x) != '.')
      if yᛌ < y
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
    @tailrec def loop(l: Vector[String], map: Map[Vector[String], Int]): Long = map.get(l) match
      case Some(to) => load(map.map(_.swap)(to + (1000000000 - map.size) % (map.size - to)))
      case None     => loop((0 until 4).foldLeft(l)((l, _) => rotate(tilt(l))), map.updated(l, map.size))
    loop(lines, Map.empty)

end Day14
