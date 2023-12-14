package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.mutable

object Day12 extends AoC:

  private def loop(
    springs: String,
    counts: List[Int],
    memo: mutable.Map[(String, Int), Long] = mutable.Map.empty
  ): Long =
    memo.getOrElseUpdate(
      springs -> counts.length,
      counts match
        case Nil if strMatch(springs, '.', springs.length) => 1L
        case Nil                                           => 0L
        case head :: tail                                  =>
          (1 to springs.length - head - tail.sum - tail.length).toList
            .filter: i =>
              strMatch(springs, '.', i) && strMatch(springs.substring(i), '#', head)
            .foldMap: i =>
              loop(springs.substring(i + head), tail, memo)
    )

  private def strMatch(s: String, c: Char, n: Int): Boolean =
    s.substring(0, n).forall(d => d == c || d == '?')

  override def part1(lines: Vector[String]): Long =
    lines.foldMap:
      case s"$springs $counts" =>
        val springs1 = "." + springs
        val counts1  = counts.split(',').map(_.toInt).toList
        loop(springs1, counts1)
  end part1

  override def part2(lines: Vector[String]): Long =
    lines.foldMap:
      case s"$springs $counts" =>
        val springs5 = "." + Array.fill(5)(springs).mkString("?")
        val counts5  = Array.fill(5)(counts.split(',').map(_.toInt)).flatten.toList
        loop(springs5, counts5)
  end part2

end Day12
