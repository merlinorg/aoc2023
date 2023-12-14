package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day13 extends AoC:
  type Area = Vector[Vector[Char]]

  private def parseAreas(lines: Vector[String]): List[Area] =
    lines.selectSplit(_.nonEmpty).map(_.toVector.map(_.toVector))

  private def differences(r0: Vector[Char], r1: Vector[Char]): Int =
    r0.zip(r1).count(_ != _)

  private def reflections(a: Area, smudges: Int): Seq[Int] =
    for
      row            <- a.indices.drop(1)
      (before, after) = a.splitAt(row)
      if smudges == before.reverse.zip(after).foldMap(differences)
    yield row

  override def part1(lines: Vector[String]): Long =
    parseAreas(lines).foldMap: area =>
      reflections(area, 0).map(_ * 100).sum + reflections(area.transpose, 0).sum

  override def part2(lines: Vector[String]): Long =
    parseAreas(lines).foldMap: area =>
      reflections(area, 1).map(_ * 100).sum + reflections(area.transpose, 1).sum

end Day13

// Part 2 in 11 lines

// Source
//  .fromResource("day-13.txt")
//  .getLines
//  .toVector
//  .selectSplit(_.nonEmpty)
//  .map(_.toVector.map(_.toVector))
//  .foldMap: area =>
//    def reflections(a: Vector[Vector[Char]]): Option[Int] =
//      a.indices.drop(1).find: row =>
//        a.take(row).reverse.zip(a.drop(row)).foldMap((r0, r1) => (r0 zip r1).count(_ != _)) == 1
//    reflections(area).foldMap(_ * 100) + reflections(area.transpose).sum
