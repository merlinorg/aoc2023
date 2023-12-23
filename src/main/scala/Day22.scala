package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day22 extends AoC:

  private final case class Block(x1: Long, y1: Long, z1: Long, x2: Long, y2: Long, z2: Long):
    def atZ1(z: Long): Block = copy(z1 = z, z2 = z2 + z - z1)

    def overlaps(o: Block): Boolean = x2 >= o.x1 && x1 <= o.x2 && y2 >= o.y1 && y1 <= o.y2
  end Block

  extension (self: Vector[String])
    private def parse: Vector[Block] =
      self.map:
        case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
          Block(x1.toLong, y1.toLong, z1.toLong, x2.toLong, y2.toLong, z2.toLong)

  extension (self: Vector[Block])
    private def fall: Vector[Block] =
      self
        .sortBy(_.z1)
        .foldLeft(Vector.empty[Block]): (result, block) =>
          val beneath = result.sortBy(-_.z2).find(_.overlaps(block))
          result :+ block.atZ1(beneath.fold(1L)(_.z2 + 1))

  override def part1(lines: Vector[String]): Long =
    val blocks = lines.parse.fall
    val slices =
      blocks.flatMap(block => (block.z1 to block.z2).map(_ -> block)).groupToMap.withDefaultValue(Vector.empty)
    blocks.count: block =>
      !slices(block.z2 + 1).exists: other =>
        other.overlaps(block) && slices(other.z1 - 1).count(other.overlaps) == 1

  override def part2(lines: Vector[String]): Long =
    val blocks = lines.parse.fall
    val slices =
      blocks.flatMap(block => (block.z1 to block.z2).map(_ -> block)).groupToMap.withDefaultValue(Vector.empty)
    blocks.foldMap: block =>
      blocks
        .foldLeft(Set(block)): (set, b) =>
          if (b.z1 > 1 && slices(b.z1 - 1).filter(b.overlaps).forall(set.contains)) set + b else set
        .size - 1
