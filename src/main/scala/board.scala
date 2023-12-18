package org.merlin.aoc2023

import scala.language.implicitConversions

type Board = Vector[String]

extension (self: Board)
  def nw: Loc               = Loc(0, 0)
  def se: Loc               = Loc(self.head.length - 1, self.length - 1)
  def apply(loc: Loc): Char = self(loc.y)(loc.x)

enum Dir(val dx: Int, val dy: Int):
  def cw: Dir                   = Dir.fromOrdinal((ordinal + 1) % 4)
  def ccw: Dir                  = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % 4)
  def reverse: Dir              = Dir.fromOrdinal((ordinal + 2) % 4)
  inline def *(scale: Int): Vec = Vec(dx * scale, dy * scale)

  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

implicit def toVec(dir: Dir): Vec = Vec(dir.dx, dir.dy)

given Ordering[Dir] = Ordering.by(_.ordinal)

final case class Vec(dx: Int, dy: Int):
  inline def *(scale: Int): Vec = Vec(dx * scale, dy * scale)

final case class Loc(x: Int, y: Int):
  inline def +(vec: Vec): Loc           = Loc(x + vec.dx, y + vec.dy)
  inline def -(vec: Vec): Loc           = Loc(x - vec.dx, y - vec.dy)
  inline def >=<(board: Board): Boolean = x >=< board.head.length && y >=< board.length

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)
