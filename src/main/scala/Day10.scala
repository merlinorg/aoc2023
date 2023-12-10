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

  private val walls = Set('|', 'J', 'L')

  override def b(lines: Vector[String]): Long =
    val w    = lines.head.length
    val path = longestLoop(lines)
    val els  = path.toSet
    val s    = path.head

    val sWall    = connections.exists:
      case (chr, dirs) =>
        walls(chr) && dirs
          .map(_ |+| s)
          .forall(p => p == path.reverse.tail.head || p == path.tail.head)
    val allWalls = if (sWall) walls + 'S' else walls

    val insides = for
      y           <- lines.indices
      (p, inside) <- (0 until w).scanLeft(-1 -> -1 -> false):
                       case ((_, inside), x) =>
                         val p      = x -> y
                         val change = els(p) && allWalls(charAt(p, lines))
                         p -> (inside ^ change)
      if inside && !els.contains(p)
    yield p

    insides.length

end Day10
