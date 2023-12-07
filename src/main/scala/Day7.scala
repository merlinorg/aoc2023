package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day7 extends AoC:
  import scala.math.Ordering.Implicits.seqOrdering

  private def f(lines: Vector[String], strengthF: String => List[Int]): Long =
    val hands  = lines.map:
      case s"$hand $bid" => hand -> bid.toLong
    val sorted = hands.map(_.leftMap(strengthF)).sortBy(_._1)
    sorted.map(_._2).zipWithIndex.foldMap((bid, rank) => (rank + 1) * bid)

  private val strengthsA = "23456789TJQKA".toList

  private def strengthA(hand: String): List[Int] =
    strengthsA.map(c => hand.count(_ == c)).sorted.reverse ::: hand.toList.map(
      strengthsA.indexOf
    )

  override def a(lines: Vector[String]): Long = f(lines, strengthA)

  private val strengthsB = "J23456789TQKA".toList

  private def strengthB(hand: String): List[Int] =
    val jokers :: cards = strengthsB.map(c => hand.count(_ == c)): @unchecked
    val best :: rest    = cards.sorted.reverse: @unchecked
    jokers + best :: rest ::: hand.toList.map(strengthsB.indexOf)

  override def b(lines: Vector[String]): Long = f(lines, strengthB)

end Day7
