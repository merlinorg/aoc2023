package org.merlin.aoc2023

import scala.io.Source
import scala.util.{Try, Using}

trait AoC {
  def a(lines: Vector[String]): Int
  def b(lines: Vector[String]): Int

  def main(args: Array[String]): Unit = {
    val num     = "\\d+".r.findFirstIn(getClass.getSimpleName).get
    val part   = if (args.contains("b")) "b" else "a"
    val sample = args.contains("sample")
    val source = Try {
      Source.fromResource(
        if (sample) s"day-$num-sample-$part.txt" else s"day-$num-$part.txt"
      )
    } getOrElse {
      Source.fromResource(if (sample) s"day-$num-sample.txt" else s"day-$num.txt")
    }
    Using.resource(source) { src =>
      val lines  = src.getLines.toVector
      val result = if (part == "a") a(lines) else b(lines)
      println(result)
    }
  }
}
