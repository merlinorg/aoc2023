package org.merlin.aoc2023

import scala.language.implicitConversions

type Board = Vector[String]

extension (self: Board)
  def nw: Loc                     = Loc(0, 0)
  def se: Loc                     = Loc(self.head.length - 1, self.length - 1)
  def apply(loc: Loc): Char       = self(loc.y)(loc.x)
  def get(loc: Loc): Option[Char] = Option.when(loc >=< self)(self(loc))

enum Dir(val dx: Int, val dy: Int):
  def cw: Dir                    = Dir.fromOrdinal((ordinal + 1) % 4)
  def ccw: Dir                   = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % 4)
  def reverse: Dir               = Dir.fromOrdinal((ordinal + 2) % 4)
  inline def *(length: Int): Vec = Vec(this, length)

  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

implicit def toVec(dir: Dir): Vec = dir * 1

given Ordering[Dir] = Ordering.by(_.ordinal)

final case class Vec(direction: Dir, length: Int):
  def dx: Int = direction.dx * length
  def dy: Int = direction.dy * length

  inline def +(delta: Int): Vec = copy(length = length + delta)
  inline def -(delta: Int): Vec = copy(length = length - delta)
  inline def *(scale: Int): Vec = copy(length = length * scale)

final case class Loc(x: Int, y: Int):
  inline def +(vec: Vec): Loc           = Loc(x + vec.dx, y + vec.dy)
  inline def -(vec: Vec): Loc           = Loc(x - vec.dx, y - vec.dy)
  inline def >=<(board: Board): Boolean = x >=< board.head.length && y >=< board.length

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)
