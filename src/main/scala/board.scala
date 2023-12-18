package org.merlin.aoc2023

import scala.language.implicitConversions

// number extensions

extension (self: Long) inline def >=<(n: Long): Boolean = self >= 0 && self < n

// iterator extensions

extension [A](self: Iterator[A]) def findMap[B](f: A => Option[B]): B = self.flatMap(f).next()

// a board is a vector of strings

type Board = Vector[String]

extension (self: Board)
  def nw: Loc                     = Origin
  def se: Loc                     = Loc(self.head.length - 1, self.length - 1)
  def apply(loc: Loc): Char       = self(loc.y.toInt)(loc.x.toInt)
  def get(loc: Loc): Option[Char] = Option.when(loc >=< self)(self(loc))

// the cardinal directions

enum Dir(val dx: Long, val dy: Long):
  def cw: Dir      = Dir.fromOrdinal((ordinal + 1) % 4)
  def ccw: Dir     = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % 4)
  def reverse: Dir = Dir.fromOrdinal((ordinal + 2) % 4)

  inline def *(length: Long): Vec = Vec(this, length)

  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

object Dir:
  val byName: Map[String, Dir] = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N).withDefault(valueOf)

  implicit def toVec(dir: Dir): Vec = dir * 1

  given Ordering[Dir] = Ordering.by(_.ordinal)

// a location in space

final case class Loc(x: Long, y: Long):
  inline def +(addend: Vec): Loc = Loc(x + addend.dx, y + addend.dy)

  inline def -(subtrahend: Vec): Loc = Loc(x - subtrahend.dx, y - subtrahend.dy)

  inline def >=<(board: Board): Boolean = x >=< board.head.length && y >=< board.length

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)

val Origin: Loc = Loc(0, 0)

// a direction and magnitude

final case class Vec(direction: Dir, magnitude: Long):
  def dx: Long = direction.dx * magnitude
  def dy: Long = direction.dy * magnitude

  inline def +(addend: Long): Vec     = copy(magnitude = magnitude + addend)
  inline def -(subtrahend: Long): Vec = copy(magnitude = magnitude - subtrahend)
  inline def *(multiplier: Long): Vec = copy(magnitude = magnitude * multiplier)
