package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day24 extends AoC:
  private final case class Hail(x0: Long, y0: Long, z0: Long, vx: Long, vy: Long, vz: Long):

    def reframed(dx: Long, dy: Long): Hail = copy(vx = vx - dx, vy = vy - dy)

    def rotate: Hail = copy(y0 = z0, z0 = y0, vy = vz, vz = vy)

    def intersect2D(h: Hail): Option[(BigDecimal, BigDecimal, BigDecimal, BigDecimal)] =
      val (a1, b1, c1) = abc
      val (a2, b2, c2) = h.abc

      val numx = b1 * c2 - b2 * c1
      val numy = c1 * a2 - c2 * a1
      val den  = a1 * b2 - a2 * b1
      Option.when(den != 0):
        val x  = numx / den
        val y  = numy / den
        val t0 = if (vx == 0) (y - y0) / vy else (x - x0) / vx
        val t1 = if (h.vx == 0) (y - h.y0) / h.vy else (x - h.x0) / h.vx
        (x, y, t0, t1)

    def abc: (BigDecimal, BigDecimal, BigDecimal) =
      (BigDecimal(vy), BigDecimal(-vx), BigDecimal(x0) * -vy + BigDecimal(y0) * vx)

  end Hail

  extension (self: Board)
    private def parse: Vector[Hail] =
      self.map:
        case s"$x, $y, $z @ $dx, $dy, $dz" =>
          Hail(x.trim.toLong, y.trim.toLong, z.trim.toLong, dx.trim.toLong, dy.trim.toLong, dz.trim.toLong)

  private def intersections(hails: Vector[Hail], min: Long, max: Long): Vector[(Hail, Hail)] =
    for
      (h0, h1)       <- hails.allPairs
      (x, y, t0, t1) <- h0.intersect2D(h1)
      if x >= min && x <= max && y >= min && y <= max && t0 >= 0 && t1 >= 0
    yield (h0, h1)

  override def part1(lines: Vector[String]): Long =
    if (lines.length == 5) intersections(lines.parse, 7, 27).size
    else intersections(lines.parse, 200000000000000L, 400000000000000L).size

  private def findOrigin(hails: Vector[Hail], dx: Long, dy: Long): Option[(Long, Long)] =
    (hails.map(_.reframed(dx, dy)): @unchecked) match
      case hail0 +: hail1 +: hail2 +: _ =>
        hail0
          .intersect2D(hail1)
          .flatMap: (x0, y0, t0, _) =>
            Option.when(
              hail2
                .intersect2D(hail0)
                .exists: (x1, y1, _, _) =>
                  x1 == x0 && y1 == y0
            ):
              (hail0.x0 + hail0.vx * t0.longValue, hail0.y0 + hail0.vy * t0.longValue)

  private final case class Spiral(loc: Loc, dir: Dir, count: Long, limit: Long):
    def next: Spiral =
      if count > 0 then copy(loc = loc + dir, count = count - 1)
      else if (dir == Dir.E || dir == Dir.W)
        copy(loc = loc + dir, dir = dir.cw, count = limit)
      else
        copy(loc = loc + dir, dir = dir.cw, count = limit + 1, limit = 1 + limit)

  private object Spiral:
    final val Start = Spiral(Origin, Dir.E, 0, 0)

  override def part2(lines: Vector[String]): Long =
    val hails  = lines.parse
    val (x, y) = Iterator
      .iterate(Spiral.Start)(_.next)
      .findMap: spiral =>
        findOrigin(hails, spiral.loc.x, spiral.loc.y)

    val rotated = hails.map(_.rotate)
    val (_, z)  = Iterator
      .iterate(Spiral.Start)(_.next)
      .findMap: spiral =>
        findOrigin(rotated, spiral.loc.x, spiral.loc.y)

    x + y + z
