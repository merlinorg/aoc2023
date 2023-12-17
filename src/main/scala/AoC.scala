package org.merlin.aoc2023

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

val NumRe: Regex = "-?\\d+".r

extension (self: Int) inline def >=<(n: Int): Boolean = self >= 0 && self < n

extension[A] (self: Iterator[A])
  def findMap[B](f: A => Option[B]): B = self.flatMap(f).next()

def readLines(day: Int, part: Int, sample: Boolean = false): Vector[String] =
  val source = Try:
    Source.fromResource(if (sample) s"day-$day-sample-$part.txt" else s"day-$day-$part.txt")
  .getOrElse:
    Source.fromResource(if (sample) s"day-$day-sample.txt" else s"day-$day.txt")
  Using.resource(source)(_.getLines.toVector)

trait AoC:
  def main(args: Array[String]): Unit =
    val day    = NumRe.findFirstIn(getClass.getSimpleName).get
    val part   = if (args.contains("2")) 2 else 1
    val sample = args.contains("sample")

    val lines  = readLines(day.toInt, part, sample)
    val result = if (part == 1) part1(lines) else part2(lines)

    println(result)
  end main

  def part1(lines: Vector[String]): Long

  def part2(lines: Vector[String]): Long
