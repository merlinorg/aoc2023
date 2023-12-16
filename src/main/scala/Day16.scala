package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.mutable

object Day16 extends AoC:
  private case class Beam(x: Int, y: Int, dx: Int, dy: Int):
    def step: Beam = copy(x = x + dx, y = y + dy)

  private def energized(lines: Vector[String], beam0: Beam): Long =
    val visited = mutable.Set.empty[(Int, Int)]
    val dedup   = mutable.Set.empty[Beam]
    val beams   = mutable.Queue(beam0)
    while (beams.nonEmpty)
      val beam = beams.dequeue()
      if (dedup.add(beam))
        val next = beam.step
        if (next.x >= 0 && next.x < lines.length && next.y >= 0 && next.y < lines.length)
          visited.add(next.x -> next.y)
          lines(next.y)(next.x) match
            case '/'                 =>
              beams.enqueue(next.copy(dx = -next.dy, dy = -next.dx))
            case '\\'                =>
              beams.enqueue(next.copy(dx = next.dy, dy = next.dx))
            case '|' if next.dx != 0 =>
              beams.enqueue(next.copy(dx = 0, dy = -1), next.copy(dx = 0, dy = 1))
            case '-' if next.dy != 0 =>
              beams.enqueue(next.copy(dx = -1, dy = 0), next.copy(dx = 1, dy = 0))
            case _                   =>
              beams.enqueue(next)
    visited.size

  override def part1(lines: Vector[String]): Long =
    energized(lines, Beam(-1, 0, 1, 0))

  override def part2(lines: Vector[String]): Long =
    lines.indices
      .flatMap: z =>
        List(Beam(-1, z, 1, 0), Beam(lines.length, z, -1, 0), Beam(z, -1, 0, 1), Beam(z, lines.length, 0, -1))
      .map: beam =>
        energized(lines, beam)
      .max

end Day16
