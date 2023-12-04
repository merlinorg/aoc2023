package org.merlin.aoc2023

object Day4 extends AoC {
  private val NumRe = "\\d+".r

  private def wins(line: String): Set[String] = line match {
    case s"Card $_: $winning | $chosen" =>
      val wins = NumRe.findAllIn(winning).toSet
      val mine = NumRe.findAllIn(chosen).toSet
      wins.intersect(mine)
  }

  private def count(i: Int, lines: Vector[String]): Int = {
    val ok = wins(lines(i))
    1 + (i + 1 to i + ok.size).map(count(_, lines)).sum
  }

  override def a(lines: Vector[String]): Int = {
    val scores = for {
      line <- lines
      ok    = wins(line)
      if ok.nonEmpty
    } yield 1 << (ok.size - 1)
    scores.sum
  }

  override def b(lines: Vector[String]): Int = {
    val results = lines.indices.map(count(_, lines))
    results.sum
  }
}
