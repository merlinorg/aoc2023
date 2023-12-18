package org.merlin.aoc2023

type Board = Vector[String]

extension (self: Board)
  def nw: Loc               = Loc(0, 0)
  def se: Loc               = Loc(self.head.length - 1, self.length - 1)
  def apply(loc: Loc): Char = self(loc.y)(loc.x)

enum Dir(val dx: Int, val dy: Int):
  def cw: Dir      = Dir.fromOrdinal((ordinal + 1) % 4)
  def ccw: Dir     = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % 4)
  def reverse: Dir = Dir.fromOrdinal((ordinal + 2) % 4)

  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

given Ordering[Dir] = Ordering.by(_.ordinal)

final case class Loc(x: Int, y: Int):
  def mulAdd(dir: Dir, count: Int): Loc = Loc(x + dir.dx * count, y + dir.dy * count)
  inline def +(dir: Dir): Loc           = Loc(x + dir.dx, y + dir.dy)
  inline def -(dir: Dir): Loc           = Loc(x - dir.dx, y - dir.dy)
  inline def >=<(board: Board): Boolean = x >=< board.head.length && y >=< board.length

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)
