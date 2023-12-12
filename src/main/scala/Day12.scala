package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.mutable

object Day12 extends AoC:

  override def a(lines: Vector[String]): Long =
    lines.foldMap:
      case s"$springs $counts" =>
        val countv                  = counts.split(',').map(_.toInt).toVector
        def loop(line: String): Int = line match
          case s"$pre?$post"                                             =>
            loop(pre + '.' + post) + loop(pre + '#' + post)
          case o if "#+".r.findAllIn(o).map(_.length).toVector == countv => 1
          case _                                                         => 0
        loop(springs)
  end a

  private def strMatch(s: String, c: Char, n: Int): Boolean =
    s.replace('?', c).startsWith(c.toString * n)

  override def b(lines: Vector[String]): Long =
    lines.foldMap:
      case s"$springs $counts" =>
        val counts5  = Array.fill(5)(counts.split(',').map(_.toInt)).flatten.toList
        val springs5 = Array.fill(5)(springs).mkString("?")
        val memo     = mutable.Map.empty[(String, Int), Long]

        def loop(springs: String, counts: List[Int]): Long =
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
                    loop(springs.substring(i + head), tail)
          )
        loop("." + springs5, counts5)
  end b

end Day12
