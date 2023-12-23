package org.merlin.aoc2023

import scala.collection.mutable

object Day23 extends AoC:
  private val prohibited = Map(Dir.W -> '>', Dir.N -> 'v', Dir.S -> '^', Dir.E -> '<')
  private type Graph = Map[Loc, Vector[(Loc, Long)]]

  extension (self: Board)
    private def junctional(loc: Loc) =
      loc.adjacents.count(adj => adj >=< self && self(adj) != '#') > 2

    // find immediate neighbouring junctions and their distances
    private def computeNeighbours(source: Loc, end: Loc): Vector[(Loc, Long)] =
      val results = mutable.ListBuffer.empty[(Loc, Long)]

      def loop(loc: Loc, visited: Set[Loc], length: Long): Unit =
        if (loc == end || junctional(loc) && loc != source)
          results.addOne(loc -> length)
        else if (!visited.contains(loc))
          for
            dir <- Dir.values
            adj  = loc + dir
            if adj >=< self && self(adj) != '#' && self(adj) != prohibited(dir)
          do loop(adj, visited + loc, length + 1)
      loop(source, Set.empty, 0)

      results.toVector

    // compute graph of all nodes to their immediate
    private def computeGraph(start: Loc, end: Loc): Graph =
      val result = mutable.Map.empty[Loc, Vector[(Loc, Long)]]

      def loop(location: Loc): Unit =
        val neighbours = computeNeighbours(location, end)
        result.update(location, neighbours)
        for
          (next, _) <- neighbours
          if !result.contains(next)
        do loop(next)
      loop(start)

      result.toMap

  override def part1(board: Board): Long =
    val start = Loc(0, 1)
    val end   = Loc(board.width - 2, board.height - 1)
    val graph = board.computeGraph(start, end)

    // try all walks
    def loop(location: Loc, visited: Set[Loc], length: Long, longest: Long): Long =
      if (location == end) length max longest
      else
        val lengths = for
          (next, distance) <- graph(location)
          if !visited.contains(next)
        yield loop(next, visited + location, length + distance, longest)
        (lengths :+ longest).max

    loop(start, Set.empty, 0, 0)

  override def part2(board: Board): Long = part1(board.map(_.replaceAll("[^#]", ".")))
