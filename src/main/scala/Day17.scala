package org.merlin.aoc2023

import scalaz.Scalaz.*

import scala.collection.mutable

object Day17 extends AoC:

  private final case class Crucible(x: Int, y: Int, dx: Int, dy: Int, count: Int, loss: Int):
    def key: (Int, Int, Int, Int, Int) = (x, y, dx, dy, count)

    def step: Crucible = copy(x = x + dx, y = y + dy, count = count + 1)

    def turn: Crucible = copy(x = x + dy, y = y + dx, dx = dy, dy = dx, count = 1)

    def turnᛌ: Crucible = copy(x = x - dy, y = y - dx, dx = -dy, dy = -dx, count = 1)

    def within(city: Vector[String]): Boolean = x >= 0 && y >= 0 && x < city.length && y < city.length

    def loseHeat(city: Vector[String]): Crucible = if (within(city)) copy(loss = loss + city(y)(x) - '0') else this

  private def solve(city: Vector[String], min: Int, max: Int): Long =
    var best    = Option.empty[Crucible]
    val visited = mutable.Map.empty[(Int, Int, Int, Int, Int), Int] // key -> loss
    val queue   = mutable.Queue(Crucible(0, 0, 1, 0, 0, 0), Crucible(0, 0, 0, 1, 0, 0))
    while (queue.nonEmpty)
      val crucible = queue.dequeue()
      if (crucible.within(city) && visited.get(crucible.key).forall(_ > crucible.loss))
        visited.update(crucible.key, crucible.loss)
        if ((crucible.x == city.length - 1) && (crucible.y == city.length - 1) && (crucible.count >= min))
          if (best.forall(_.loss > crucible.loss)) best = Some(crucible)
        else
          if (crucible.count < max) queue.enqueue(crucible.step.loseHeat(city))
          if (crucible.count >= min) queue.enqueue(crucible.turn.loseHeat(city), crucible.turnᛌ.loseHeat(city))
    best.cata(_.loss, 0)

  override def part1(city: Vector[String]): Long = solve(city, 0, 3)

  override def part2(city: Vector[String]): Long = solve(city, 4, 10)

end Day17
