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
  @tailrec private def partition(graph: Graph, vertices: Set[String]): Set[String] =
    if vertices.foldMap(graph(_).count(!vertices(_))) == 3 then vertices
    else partition(graph, vertices + (graph.keySet -- vertices).minBy(graph(_).count(!vertices(_))))

  override def part1(lines: Vector[String]): Long =
    val graph = lines.parse
    val size  = partition(graph, Set(graph.keySet.head)).size
    size * (graph.size - size)

  override def part2(lines: Vector[String]): Long = 0
}
