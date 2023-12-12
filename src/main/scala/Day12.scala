package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day12 extends AoC:

  def countBroken(s: String): Vector[Int] =
    "#+".r.findAllIn(s).map(_.length).toVector

  override def a(lines: Vector[String]): Long =
    lines.foldMap:
      case s"$springs $orders" =>
        val counts                  = orders.split(',').map(_.toInt).toVector
        def loop(line: String): Int = {
          val idx = line.indexOf('?')
          if (idx < 0) {
            if (countBroken(line) == counts) 1 else 0
          } else {
            val s2 = line.substring(0, idx)
            val s3 = line.substring(1 + idx)
            loop(s2 + '.' + s3) + loop(s2 + '#' + s3)
          }
        }
        loop(springs)
  end a

  @tailrec private def unmatch(s: String, c: Char, i: Int, n: Int): Boolean =
    if (n == 0)
      true
    else if (s.charAt(i) == c)
      false
    else
      unmatch(s, c, i + 1, n - 1)

  override def b(lines: Vector[String]): Long =
    lines.foldMap:
      case s"$springs $counts" =>
        val counts5  =
          Array.fill(5)(counts.split(',').map(_.toInt)).flatten.toList
        val springs5 = Array.fill(5)(springs).mkString("?")

        val memo = mutable.Map.empty[(Int, Int), Long]

        def loop(index: Int, counts: List[Int]): Long =
          memo.getOrElseUpdate(
            index -> counts.length,
            counts match
              case Nil if unmatch(springs5, '#', index, springs5.length - index) =>
                1L
              case Nil                                                           =>
                0L
              case head :: tail                                                  =>
                val start = if (index == 0) 0 else 1
                val count = springs5.length - index - head - tail.sum - tail.length
                (start to count).toList.foldMap: i =>
                  if (unmatch(springs5, '#', index, i) && unmatch(springs5, '.', index + i, head))
                    loop(index + i + head, tail)
                  else 0L
          )
        loop(0, counts5)
  end b

end Day12
