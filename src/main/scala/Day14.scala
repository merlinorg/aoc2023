package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day14 extends AoC:
  private type Area = Vector[String]
  
  private def tilt(lines: Area): Area =
    lines.map: line =>
      "#+|[^#]+".r.findAllIn(line).map(_.sortBy(_ != 'O')).mkString

  private def load(lines: Area): Long =
    lines.foldMap: line =>
      line.toList.zipWithIndex.foldMap:
        case (c, i) => if (c == 'O') lines.length - i else 0

  private def rotate(lines: Area): Area =
    lines.transpose.map(_.reverse.mkString)

  private def rotateᛌ(lines: Area): Area =
    lines.map(_.reverse).transpose.map(_.mkString)

  override def part1(lines: Area): Long =
    load(tilt(rotateᛌ(lines)))

  override def part2(lines: Area): Long =
    @tailrec def loop(l: Area, map: Map[Area, Int]): Area = map.get(l) match
      case Some(to) => map.map(_.swap)(to + (1000000000 - map.size) % (map.size - to))
      case None     => loop((0 until 4).foldLeft(l)((l, _) => rotate(tilt(l))), map.updated(l, map.size))
    load(loop(rotateᛌ(lines), Map.empty))

end Day14

// Part 2 in 11 lines

// def tilt(lines: Vector[String]): Vector[String] =
//   lines.map: line =>
//     "#+|[^#]+".r.findAllIn(line).map(_.sortBy(_ != 'O')).mkString
//
// @tailrec def loop(l: Vector[String], map: Map[Vector[String], Int]): Vector[String] = map.get(l) match
//   case Some(to) => map.map(_.swap)(to + (1000000000 - map.size) % (map.size - to))
//   case None     =>
//     loop((0 until 4).foldLeft(l)((l, _) => tilt(l).transpose.map(_.reverse.mkString)), map.updated(l, map.size))
//
// val lines = Source.fromResource("day-14.txt").getLines.toVector.map(_.reverse).transpose.map(_.mkString)
//
// loop(lines, Map.empty).foldMap: line =>
//   line.toList.zipWithIndex.foldMap:
//     case (c, i) => if (c == 'O') line.length - i else 0
