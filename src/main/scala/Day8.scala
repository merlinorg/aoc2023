package org.merlin.aoc2023

import scala.annotation.tailrec

object Day8 extends AoC:
  private def parseRules(
    lines: Vector[String]
  ): Map[String, Map[Char, String]] =
    lines.foldLeft(Map.empty[String, Map[Char, String]]):
      case (acc, s"$from = ($l, $r)") =>
        acc.updated(from, Map('L' -> l, 'R' -> r))

  override def a(lines: Vector[String]): Long =
    val lr    = LazyList.continually(lines.head).flatten
    val rules = parseRules(lines.drop(2))

    lr.scanLeft("AAA"): (pos, dir) =>
      rules(pos)(dir)
    .indexOf("ZZZ")
  end a

  @tailrec private def gcd(x: Long, y: Long): Long =
    if (y == 0) x else gcd(y, x % y)

  private def lcm(list: Iterable[Long]): Long =
    list.foldLeft(1L): (a, b) =>
      b * a / gcd(a, b)

  override def b(lines: Vector[String]): Long =
    val lr    = LazyList.continually(lines.head).flatten
    val rules = parseRules(lines.drop(2))

    def count(start: String): Long =
      lr.scanLeft(start): (pos, dir) =>
        rules(pos)(dir)
      .indexWhere(_.endsWith("Z"))

    lcm(rules.keys.filter(_.endsWith("A")).map(count))
  end b

end Day8
