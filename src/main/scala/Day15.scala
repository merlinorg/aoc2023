package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day15 extends AoC:
  private type Lens = (String, Int)

  extension (self: String)
    private def hash: Int =
      self.foldLeft(0)((h, c) => ((h + c) * 17) % 256)

  extension (self: List[Lens])
    private def remove(label: String): List[Lens] =
      self.filterNot(_._1 == label)

    private def add(label: String, focus: Int): List[Lens] =
      if (self.exists(_._1 == label)) self.map(lens => if (lens._1 == label) label -> focus else lens)
      else self :+ (label -> focus)

  override def part1(lines: Vector[String]): Long =
    lines.flatMap(_.split(",")).foldMap(_.hash)

  override def part2(lines: Vector[String]): Long =
    lines
      .flatMap(_.split(","))
      .foldLeft(Map.empty[Int, List[Lens]]):
        case (boxes, s"$label-")       =>
          boxes.updatedWith(label.hash)(_.map(_.remove(label)))
        case (boxes, s"$label=$focus") =>
          boxes.updatedWith(label.hash)(_.map(_.add(label, focus.toInt)).orElse(Some(List(label -> focus.toInt))))
      .toList
      .foldMap:
        case (box, lenses) =>
          lenses.zipWithIndex.foldMap((lens, index) => (box + 1) * (index + 1) * lens._2)

end Day15
