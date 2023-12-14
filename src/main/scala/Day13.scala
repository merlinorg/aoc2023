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

  override def a(lines: Vector[String]): Long =
    parseAreas(lines).foldMap: area =>
      reflections(area, 0).map(_ * 100).sum + reflections(area.transpose, 0).sum

  override def b(lines: Vector[String]): Long =
    parseAreas(lines).foldMap: area =>
      reflections(area, 1).map(_ * 100).sum + reflections(area.transpose, 1).sum

// 1.  Source
// 2.   .fromResource("day-13.txt")
// 3.   .getLines
// 4.   .toVector
// 5.   .selectSplit(_.nonEmpty)
// 6.   .map(_.toVector.map(_.toVector))
// 7.   .foldMap: area =>
// 8.     def reflections(a: Vector[Vector[Char]]): Option[Int] =
// 9.       a.indices.drop(1).find: row =>
// A.         a.take(row).reverse.zip(a.drop(row)).foldMap((r0, r1) => (r0 zip r1).count(_ != _)) == 1
// B.     reflections(area).foldMap(_ * 100) + reflections(area.transpose).sum

end Day13
