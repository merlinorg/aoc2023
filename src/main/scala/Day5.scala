package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day5 extends AoC:
  private final case class Mapping(dst: Long, src: Long, len: Long):
    def maps(value: Long): Boolean     = value >= src && value < src + len
    def mapsTo(value: Long): Boolean   = value >= dst && value < dst + len
    def map(value: Long): Option[Long] = maps(value).option(value + dst - src)
    def take(n: Long): Mapping         = copy(len = n)
    def drop(n: Long): Mapping         = copy(dst = dst + n, src = src + n, len - n)
  end Mapping

  @tailrec private def parseMappings(
    input: List[String],
    mappings: List[Mapping] = Nil
  ): (List[Mapping], List[String]) = input match
    case s"$dst $src $len" :: rest =>
      val mapping = Mapping(dst.toLong, src.toLong, len.toLong)
      parseMappings(
        rest,
        mapping :: mappings,
      )
    case "" :: rest                => (mappings, rest)
    case _                         => (mappings, Nil)

  @tailrec private def parseAllMappings(
    input: List[String],
    list: List[List[Mapping]] = Nil
  ): List[List[Mapping]] = input match
    case s"$from-to-$to map:" :: rest =>
      val (mappings, remainder) = parseMappings(rest)
      parseAllMappings(
        remainder,
        mappings :: list,
      )
    case _                            => list.reverse

  private def transform(value: Long, mappings: List[Mapping]): Long =
    mappings.findMapM[Id, Long](_.map(value)) | value

  private def combine(as: List[Mapping], bs: List[Mapping]): List[Mapping] =
    val aPoints = as.flatMap(a => a.src :: (a.dst + a.len) :: Nil)
    val bPoints = bs.flatMap(b => b.src :: (b.src + b.len) :: Nil)
    val aSplit  = bPoints.foldLeft(as): (mappings, point) =>
      mappings.flatMap: range =>
        split(range, point - range.dst)
    val bSplit  = aPoints.foldLeft(bs): (mappings, point) =>
      mappings.flatMap: range =>
        split(range, point - range.src)
    aSplit.map(a => a.copy(dst = transform(a.dst, bs))) ++
      bSplit.filterNot(b => as.exists(_.mapsTo(b.src)))

  private def split(range: Mapping, n: Long): List[Mapping] =
    if (n > 0 && n < range.len) range.take(n) :: range.drop(n) :: Nil
    else range :: Nil

  override def part1(lines: Vector[String]): Long =
    val seeds        = NumRe.findAllIn(lines.head).map(_.toLong).toList
    val head :: tail = parseAllMappings(lines.drop(2).toList): @unchecked
    val mappings     = tail.foldLeft(head)(combine)
    seeds.map(transform(_, mappings)).min
  end part1

  override def part2(lines: Vector[String]): Long =
    val pairs        = NumRe.findAllIn(lines.head).map(_.toLong).toList.grouped(2)
    val head :: tail = parseAllMappings(lines.drop(2).toList): @unchecked
    val mappings     = tail.foldLeft(head)(combine)
    val seeds        =
      pairs.toList.map(pair => Mapping(pair.head, pair.head, pair.last))
    val values       = seeds.map(seed => transform(seed.src, mappings)) ++
      mappings.filter(mapping => seeds.exists(_.maps(mapping.src))).map(_.dst)
    values.min
  end part2

end Day5
