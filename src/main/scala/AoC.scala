package org.merlin.aoc2023

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

trait AoC:

  protected val NumRe: Regex = "-?\\d+".r

  def part1(lines: Vector[String]): Long

  def part2(lines: Vector[String]): Long

  def main(args: Array[String]): Unit =

    val num    = NumRe.findFirstIn(getClass.getSimpleName).get
    val part   = if (args.contains("b")) "b" else "a"
    val sample = args.contains("sample")

    val source = Try:
      Source.fromResource(
        if (sample) s"day-$num-sample-$part.txt" else s"day-$num-$part.txt"
      )
    .getOrElse:
      Source.fromResource(
        if (sample) s"day-$num-sample.txt" else s"day-$num.txt"
      )

    Using.resource(source): src =>
      val lines  = src.getLines.toVector
      val result = if (part == "a") part1(lines) else part2(lines)
      println(result)

  end main

end AoC
