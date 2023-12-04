package org.merlin.aoc2023

import scalaz.Monoid
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.foldable._

object Day2 extends AoC {
  private def isPlausible(round: Map[String, Int]): Boolean =
    round("red") <= 12 && round("green") <= 13 && round("blue") <= 14

  private def parse(line: String): (Int, List[Map[String, Int]]) = line match {
    case s"Game $id: $results" =>
      val rounds = for {
        round <- results.split("; ").toList
        pairs  = round.split(", ")
        tuples = pairs collect { case s"$count $colour" =>
                   colour -> count.toInt
                 }
      } yield tuples.toMap.withDefaultValue(0)
      id.toInt -> rounds
  }

  override def a(lines: Vector[String]): Int = {
    val possibilities = lines.map(parse) collect {
      case (id, rounds) if rounds.forall(isPlausible) => id
    }
    possibilities.sum
  }

  override def b(lines: Vector[String]): Int = {
    implicit val maxMonoid: Monoid[Int] = Monoid.instance(_ max _, 0)

    val powers = lines.map(parse) map { case (id, rounds) =>
      val minima = rounds.suml
      minima("red") * minima("green") * minima("blue")
    }
    powers.sum
  }

}
