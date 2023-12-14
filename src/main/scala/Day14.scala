package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day14 extends AoC:
  private type Dish = Vector[String]

  extension (self: Dish)
    private def tilt: Dish =
      self.map(line => "#+|[^#]+".r.findAllIn(line).map(_.sortBy(_ != 'O')).mkString)

    private def rotate: Dish =
      self.transpose.map(_.reverse.mkString)

    private def rotateᛌ: Dish =
      self.map(_.reverse).transpose.map(_.mkString)

    @tailrec private def cycle(n: Int, d: Dish = self, map: Map[Dish, Int] = Map.empty): Dish = map.get(d) match
      case Some(to) => map.map(_.swap)(to + n % (map.size - to))
      case None     => cycle(n - 1, d.tilt.rotate.tilt.rotate.tilt.rotate.tilt.rotate, map.updated(d, map.size))

    private def load: Long =
      self.foldMap: line =>
        line.toList.zipWithIndex.foldMap:
          case (c, i) => if (c == 'O') line.length - i else 0

  override def part1(dish: Dish): Long =
    dish.rotateᛌ.tilt.load

  override def part2(dish: Dish): Long =
    dish.rotateᛌ.cycle(1_000_000_000).load

end Day14

// Part 2 in 10 lines

// def tilt(lines: Vector[String]): Vector[String] =
//   lines.map(line => "#+|[^#]+".r.findAllIn(line).map(_.sortBy(_ != 'O')).mkString)
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
