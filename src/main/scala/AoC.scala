package org.merlin.aoc2023

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

trait AoC:

  protected val NumRe: Regex = "\\d+".r

  def a(lines: Vector[String]): Int

  def b(lines: Vector[String]): Int

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
      val result = if (part == "a") a(lines) else b(lines)
      println(result)

  end main

end AoC
