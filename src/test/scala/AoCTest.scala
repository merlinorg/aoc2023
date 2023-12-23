package org.merlin.aoc2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AoCTest extends AnyFreeSpec with Matchers {
  test(Day1, 142, 54561, 281, 54076)
  test(Day2, 8, 2593, 2286, 54699)
  test(Day3, 4361, 528819, 467835, 80403602)
  test(Day4, 13, 32001, 30, 5037841)
  test(Day5, 35, 424490994, 46, 15290096)
  test(Day6, 288, 2065338, 71503, 34934171)
  test(Day7, 6440, 255048101, 5905, 253718286)
  test(Day8, 6, 21883, 6, 12833235391111L)
  test(Day9, 114, 1702218515, 2, 925)
  test(Day10, 8, 6956, 10, 455)
  test(Day11, 374, 9522407, 82000210, 544723432977L)
  test(Day12, 21, 7118, 525152, 7030194981795L)
  test(Day13, 405, 34202, 400, 34230)
  test(Day14, 136, 109833, 64, 99875)
  test(Day15, 1320, 503154, 145, 251353)
  test(Day16, 46, 7067, 51, 7324)
  test(Day17, 102, 1155, 94, 1283)
  test(Day17FP, 102, 1155, 94, 1283)
  test(Day17FPAlt, 102, 1155, 94, 1283)
  test(Day18, 62, 46394, 952408144115L, 201398068194715L)
  test(Day18Alt, 62, 46394, 952408144115L, 201398068194715L)
  test(Day19, 19114, 425811, 167409079868000L, 131796824371749L)
  test(Day20, 11687500, 944750144, 1, 222718819437131L)
  test(Day21, 16, 3768, 167004, 627960775905777L)
  test(Day22, 5, 497, 7, 67468)
  test(Day23, 94, 2070, 154, 6498)

  def test(day: AoC, sample1: Long, answer1: Long, sample2: Long, answer2: Long): Unit = {
    val name = day.getClass.getSimpleName.dropRight(1)
    val num = "\\d+".r.findFirstIn(name).get.toInt
    name - {
      "Part 1" - {
        "Sample Input" in {
          day.part1(readLines(num, 1, sample = true)) shouldBe sample1
        }
        "Real Input" in {
          day.part1(readLines(num, 1)) shouldBe answer1
        }
      }
      "Part 2" - {
        "Sample Input" in {
          day.part2(readLines(num, 2, sample = true)) shouldBe sample2
        }
        "Real Input" in {
          day.part2(readLines(num, 2)) shouldBe answer2
        }
      }
    }
  }
}
