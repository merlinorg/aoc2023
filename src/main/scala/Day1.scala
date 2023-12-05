package org.merlin.aoc2023

object Day1 extends AoC:

  private val Digits = List(
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  )

  private val DigRe = ("[0-9]" :: Digits).mkString("|").r

  override def a(lines: Vector[String]): Long =
    val calibrations = for
      line  <- lines
      first <- line.find(_.isDigit)
      last  <- line.findLast(_.isDigit)
    yield s"$first$last".toInt

    calibrations.sum
  end a

  override def b(lines: Vector[String]): Long =
    val calibrations = for
      line      <- lines
      matches    = DigRe.findAllIn(line).toList
      first     <- matches.headOption
      firstIndex = Digits.indexOf(first)
      firstDigit = if (firstIndex < 0) first.toInt else firstIndex
      last      <- matches.lastOption
      lastIndex  = Digits.indexOf(last)
      lastDigit  = if (lastIndex < 0) last.toInt else lastIndex
    yield s"$firstDigit$lastDigit".toInt

    calibrations.sum
  end b

end Day1