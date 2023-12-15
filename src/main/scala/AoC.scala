package org.merlin.aoc2023

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

trait AoC:

  protected val NumRe: Regex = "-?\\d+".r

  def part1(lines: Vector[String]): Long

  def part2(lines: Vector[String]): Long

  def main(args: Array[String]): Unit =
    val day    = NumRe.findFirstIn(getClass.getSimpleName).get
    val part   = if (args.contains("2")) 2 else 1
    val sample = args.contains("sample")

    val lines  = AoC.readLines(day.toInt, part, sample)
    val result = if (part == 1) part1(lines) else part2(lines)

    println(result)
  end main

end AoC

object AoC:
  def readLines(day: Int, part: Int, sample: Boolean = false): Vector[String] = {
    val source = Try:
      Source.fromResource(
        if (sample) s"day-$day-sample-$part.txt" else s"day-$day-$part.txt"
      )
    .getOrElse:
      Source.fromResource(
        if (sample) s"day-$day-sample.txt" else s"day-$day.txt"
      )
    Using.resource(source)(_.getLines.toVector)
  }
end AoC
