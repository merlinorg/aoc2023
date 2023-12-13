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

end Day13
