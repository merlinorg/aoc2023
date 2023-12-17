package org.merlin.aoc2023

import scalaz.Scalaz.*

import scala.collection.SortedSet

object Day17FP extends AoC:
  private type City = Vector[String]

  private final case class Crucible(x: Int, y: Int, dx: Int, dy: Int, count: Int):
    def forward: Crucible = copy(x = x + dx, y = y + dy, count = count + 1)
    def turn: Crucible    = copy(x = x + dy, y = y + dx, dx = dy, dy = dx, count = 1)
    def turnᛌ: Crucible   = copy(x = x - dy, y = y - dx, dx = -dy, dy = -dx, count = 1)

    def moves(min: Int, max: Int): List[Crucible] =
      (count < max) ?? List(forward) ++ (count >= min) ?? List(turn, turnᛌ)

    def within(city: City): Boolean = x >=< city.head.length && y >=< city.length

  private given Ordering[Crucible] = Ordering.by(Tuple.fromProductTyped)

  private final case class State(queue: SortedSet[(Int, Crucible)], seen: Set[Crucible]):
    def update(city: City, min: Int, max: Int): State =
      val (loss, crucible) = queue.head

      val moves = for
        moved   <- crucible.moves(min, max) if moved.within(city) && !seen.contains(moved)
        heatLoss = loss + city(moved.y)(moved.x).asDigit
      yield heatLoss -> moved

      State(queue.tail ++ moves, seen ++ moves.map(_._2))

    def solution(city: City, min: Int): Option[Long] =
      queue.headOption.collect:
        case (heatLoss, c) if c.x == city.head.length - 1 && c.y == city.length - 1 && c.count >= min => heatLoss

  private def solve(city: City, min: Int, max: Int): Long =
    Iterator
      .iterate(State(SortedSet(0 -> Crucible(0, 0, 1, 0, 0), 0 -> Crucible(0, 0, 0, 1, 0)), Set.empty)): state =>
        state.update(city, min, max)
      .findMap(_.solution(city, min))

  override def part1(city: City): Long = solve(city, 0, 3)

  override def part2(city: City): Long = solve(city, 4, 10)
