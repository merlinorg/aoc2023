package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day7 extends AoC:
  private val strengthsA =
    List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

  private def strengthA(hand: String): List[Int] =
    strengthsA.map(c => hand.count(_ == c)).sorted.reverse

  private def compare(
    hand0: String,
    hand1: String,
    strengthF: String => List[Int],
    strengths: List[Char]
  ): Boolean =
    val card0 = strengthF(hand0)
    val card1 = strengthF(hand1)
    val ord   = card0 cmp card1
    (ord == Ordering.LT) || (ord == Ordering.EQ) && hand0
      .zip(hand1)
      .find((c0, c1) => c0 != c1)
      .exists((c0, c1) => strengths.indexOf(c0) > strengths.indexOf(c1))

  override def a(lines: Vector[String]): Long =
    val hands  = lines.map:
      case s"$hand $bid" => hand -> bid.toLong
    val sorted = hands.sortWith:
      case ((a0, _), (a1, _)) => compare(a0, a1, strengthA, strengthsA)
    val scored = sorted.zipWithIndex.map:
      case ((_, bid), rank) => (rank + 1) * bid
    scored.sum
  end a

  private val strengthsB =
    List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')

  private def strengthB(hand: String): List[Int] =
    val jokers = hand.count(_ == 'J')
    val best :: rest = strengthsB.filter(_ != 'J').map(c => hand.count(_ == c)).sorted.reverse: @unchecked
    jokers + best :: rest

  override def b(lines: Vector[String]): Long =
    val hands = lines.map:
      case s"$hand $bid" => hand -> bid.toLong
    val sorted = hands.sortWith:
      case ((a0, _), (a1, _)) => compare(a0, a1, strengthB, strengthsB)
    val scored = sorted.zipWithIndex.map:
      case ((_, bid), rank) => (rank + 1) * bid
    scored.sum
  end b

end Day7
