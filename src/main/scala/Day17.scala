package org.merlin.aoc2023

import scalaz.Scalaz.*

import scala.collection.mutable

object Day17 extends AoC:

  private final case class Crucible(x: Int, y: Int, dx: Int, dy: Int, count: Int, loss: Int):
    def key: (Int, Int, Int, Int, Int) = (x, y, dx, dy, count)

    def forward: Crucible = copy(x = x + dx, y = y + dy, count = count + 1)

    def turn: Crucible = copy(x = x + dy, y = y + dx, dx = dy, dy = dx, count = 1)

    def turnᛌ: Crucible = copy(x = x - dy, y = y - dx, dx = -dy, dy = -dx, count = 1)

    def within(city: Vector[String]): Boolean = x >= 0 && y >= 0 && x < city.head.length && y < city.length

    def loseHeat(city: Vector[String]): Crucible = copy(loss = loss + within(city) ?? city(y)(x).asDigit)

  private given Ordering[Crucible] = Ordering.by(x => -x.loss)

  private def solve(city: Vector[String], min: Int, max: Int): Long =
    val seen  = mutable.Map.empty[(Int, Int, Int, Int, Int), Int] // ლ(ಠ益ಠლ) key -> loss
    val queue = mutable.PriorityQueue(Crucible(0, 0, 1, 0, 0, 0), Crucible(0, 0, 0, 1, 0, 0))
    while (queue.head.x != city.head.length - 1 || queue.head.y != city.length - 1 || queue.head.count < min)
      val crucible = queue.dequeue()
      if (seen.get(crucible.key).forall(_ > crucible.loss) && crucible.within(city))
        seen.update(crucible.key, crucible.loss)
        if (crucible.count < max) queue.enqueue(crucible.forward.loseHeat(city))
        if (crucible.count >= min) queue.enqueue(crucible.turn.loseHeat(city), crucible.turnᛌ.loseHeat(city))
    queue.head.loss

  override def part1(city: Vector[String]): Long = solve(city, 0, 3)

  override def part2(city: Vector[String]): Long = solve(city, 4, 10)

end Day17
