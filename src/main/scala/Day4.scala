package org.merlin.aoc2023

import scalaz.Scalaz._

object Day4 extends AoC:

  private def wins(line: String): Int = line match
    case s"Card $_: $winning | $chosen" =>
      val wins = NumRe.findAllIn(winning).toSet
      val mine = NumRe.findAllIn(chosen).toSet
      wins.intersect(mine).size

  override def a(lines: Vector[String]): Int =
    val scores = for
      line <- lines
      ok    = wins(line)
      if ok > 0
    yield 1 << (ok - 1)

    scores.sum
  end a

  override def b(lines: Vector[String]): Int =
    val winMap             = lines.zipWithIndex.map(_.swap.rightMap(wins)).toMap
    def count(i: Int): Int = 1 + (i + 1 to i + winMap(i)).map(count).sum
    lines.indices.map(count).sum
  end b

end Day4
