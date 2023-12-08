package org.merlin.aoc2023

import scala.annotation.tailrec

object Day8 extends AoC:
  private def parse(lines: Vector[String]): (Iterator[Char], Map[String, Map[Char, String]]) =
    val llr = Iterator.continually(lines.head).flatten
    val rules = lines
      .drop(2)
      .map:
        case s"$x = ($l, $r)" =>
          x -> Map('L' -> l, 'R' -> r)
      .toMap
    llr -> rules

  override def a(lines: Vector[String]): Long =
    val (llr, rules) = parse(lines)

    @tailrec def loop(from: String, lr: Iterator[Char], count: Long): Long =
      val next = rules(from)(lr.next())
      if (next == "ZZZ") count else loop(next, lr, 1 + count)

    loop("AAA", llr, 1)
  end a

  // https://stackoverflow.com/questions/40875537/fp-lcm-in-scala-in-1-line
  private def lcm(list: Iterable[Long]): Long =
    list.foldLeft(1: Long): (a, b) =>
      b * a / LazyList
        .iterate((a, b)) { case (x, y) => (y, x % y) }
        .dropWhile(_._2 != 0)
        .head
        ._1
        .abs

  override def b(lines: Vector[String]): Long =
    val (llr, rules) = parse(lines)

    @tailrec def loop(from: String, lr: Iterator[Char], count: Long): Long =
      val next = rules(from)(lr.next())
      if (next.endsWith("Z")) count else loop(next, lr, 1 + count)

    lcm(rules.keys.filter(_.endsWith("A")).map(loop(_, llr, 1)))
  end b

end Day8
