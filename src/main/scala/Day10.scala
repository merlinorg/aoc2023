package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day10 extends AoC:
  private type Point = (Int, Int)

  private val N = 0  -> -1
  private val E = 1  -> 0
  private val S = 0  -> 1
  private val W = -1 -> 0

  private val connections = Map[Char, List[Point]](
    'S' -> List(N, E, S, W),
    '|' -> List(N, S),
    '-' -> List(W, E),
    'L' -> List(N, E),
    'J' -> List(W, N),
    '7' -> List(S, W),
    'F' -> List(E, S),
  ).withDefaultValue(Nil)

  private def charAt(p: Point, lines: Vector[String]): Char =
    lines.unapply(p._2).flatMap(_.unapply(p._1)) | ' '

  private def longestLoop(lines: Vector[String]): List[Point] =
    val sy = lines.indexWhere(_.contains('S'))
    val sx = lines(sy).indexOf('S')
    val s  = (sx, sy)

    val paths = Iterator.iterate(List(s)): path =>
      val pos :: tail = path: @unchecked
      val nxts        = for
        fwd <- connections(charAt(pos, lines))
        nxt  = fwd |+| pos
        rev <- connections(charAt(nxt, lines))
        if pos == (rev |+| nxt) && !tail.headOption.contains(nxt)
      yield nxt
      nxts.head :: path

    paths.drop(1).find(_.head == s).get
  end longestLoop

  override def a(lines: Vector[String]): Long =
    longestLoop(lines).length / 2
  end a

  // I ought to see what S is and add to this list if necessary sigh
  private val walls = Set('|', 'J', 'L')

  override def b(lines: Vector[String]): Long =
    val w = lines.head.length
    val h = lines.length

    val path = longestLoop(lines).toSet

    val insides = for
      y           <- 0 until h
      (p, inside) <- (0 until w).scanLeft(-1 -> -1 -> false):
                       case ((_, inside), x) =>
                         val chr    = charAt(x -> y, lines)
                         val change = path(x -> y) && walls(chr)
                         x -> y -> (inside ^ change)
      if inside && !path.contains(p)
    yield p

    insides.length

end Day10
