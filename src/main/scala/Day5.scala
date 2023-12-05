package org.merlin.aoc2023

import scalaz.Scalaz.*

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Day5 extends AoC:
  // start - 1 -> range
  private type Ranges = SortedMap[Long, Range]
  // from -> to, ranges
  private type Maps   = Map[String, (String, Ranges)]

  private final case class Range(dst: Long, src: Long, len: Long) {
    def contains(value: Long): Boolean = value >= src && value < src + len
    def apply(value: Long): Long       = value + dst - src
  }

  @tailrec private def parseRanges(
    l: List[String],
    ranges: Ranges = SortedMap.empty
  ): (Ranges, List[String]) = l match
    case s"$dst $src $len" :: rest =>
      val range = Range(dst.toLong, src.toLong, len.toLong)
      parseRanges(
        rest,
        ranges.updated(range.src - 1, range),
      )
    case "" :: rest                => (ranges, rest)
    case _                         => (ranges, Nil)

  @tailrec private def parseMaps(
    l: List[String],
    maps: Maps = Map.empty
  ): Maps = l match
    case s"$from-to-$to map:" :: rest =>
      val (ranges, remainder) = parseRanges(rest)
      parseMaps(
        remainder,
        maps.updated(from, to -> ranges),
      )
    case _                            => maps

  private def findRange(value: Long, ranges: Ranges): Option[Range] =
    ranges.maxBefore(value).map(_._2).filter(_.contains(value))

  @tailrec private def location(value: Long, from: String, maps: Maps): Long =
    maps.get(from) match
      case Some((to, ranges)) =>
        val result = findRange(value, ranges).cata(_.apply(value), value)
        location(result, to, maps)
      case None               => value

  override def a(lines: Vector[String]): Long =
    val seeds = NumRe.findAllIn(lines.head).map(_.toLong).toList
    val maps  = parseMaps(lines.drop(2).toList)
    seeds.map(location(_, "seed", maps)).min
  end a

  // brute force. todo: just combine all the ranges
  override def b(lines: Vector[String]): Long =
    val pairs = NumRe.findAllIn(lines.head).map(_.toLong).toList.grouped(2)
    val maps  = parseMaps(lines.drop(2).toList)
    val seeds = pairs.flatMap(pair => pair.head to pair.head + pair.last)
    seeds.map(location(_, "seed", maps)).min
  end b

end Day5
