package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day25 extends AoC {
  type Graph = Map[String, Vector[String]]

  extension (self: Vector[String])
    private def parse: Graph =
      val edges = self.flatMap:
        case s"$lhs: $rhss" => rhss.split(" ").map(lhs -> _)
      (edges ++ edges.map(_.swap)).groupToMap

  // want https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm but this seems to work shrug
  override def part1(lines: Vector[String]): Long =
    val graph = lines.parse

    @tailrec def loop(group: Set[String]): Long =
      if group.foldMap(graph(_).count(!group(_))) == 3 then group.size
      else loop(group + (graph.keySet -- group).minBy(graph(_).count(!group(_))))

    val size = loop(Set(graph.keySet.head))
    size * (graph.size - size)

  override def part2(lines: Vector[String]): Long = 0
}
