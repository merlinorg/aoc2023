package org.merlin.aoc2023

import scalaz.Scalaz.*

import scala.collection.SortedSet

object Day17FP extends AoC:
  private type City = Vector[String]

  private type Key = (Int, Int, Int, Int, Int)

  private final case class Crucible(loss: Int, x: Int, y: Int, dx: Int, dy: Int, count: Int):
    def key: Key = Tuple.fromProductTyped(this).drop(1)

    def forward: Crucible = copy(x = x + dx, y = y + dy, count = count + 1)

    def turn: Crucible = copy(x = x + dy, y = y + dx, dx = dy, dy = dx, count = 1)

    def turnᛌ: Crucible = copy(x = x - dy, y = y - dx, dx = -dy, dy = -dx, count = 1)

    def cooled(amount: Int): Crucible = copy(loss = loss + amount)

    def moves(min: Int, max: Int): List[Crucible] =
      (count < max) ?? List(forward) ++ (count >= min) ?? List(turn, turnᛌ)

    def within(city: City): Boolean = x >=< city.head.length && y >=< city.length

  private given Ordering[Crucible] = Ordering.by(Tuple.fromProductTyped)

  private final case class State(queue: SortedSet[Crucible], visited: Map[Key, Int]):
    def update(city: City, min: Int, max: Int): State =
      val crucibles = for
        moved <- queue.head.moves(min, max) if moved.within(city)
        cooled = moved.cooled(city(moved.y)(moved.x).asDigit) if visited.get(cooled.key).forall(_ > cooled.loss)
      yield cooled
      copy(queue = queue.tail ++ crucibles, visited = visited ++ crucibles.map(c => c.key -> c.loss))

    def finisher(city: City, min: Int): Option[Crucible] =
      queue.headOption.filter(c => c.x == city.head.length - 1 && c.y == city.length - 1 && c.count >= min)
  
  private def solve(city: City, min: Int, max: Int): Long =
    Iterator
      .iterate(State(SortedSet(Crucible(0, 0, 0, 1, 0, 0), Crucible(0, 0, 0, 0, 1, 0)), Map.empty)): state =>
        state.update(city, min, max)
      .findMap(_.finisher(city, min))
      .loss

  override def part1(city: City): Long = solve(city, 0, 3)

  override def part2(city: City): Long = solve(city, 4, 10)

end Day17FP
