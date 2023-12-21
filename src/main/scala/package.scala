package org.merlin.aoc2023

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

// number extensions

extension (self: Int)
  inline def %%(n: Int): Int =
    val mod = self % n
    if mod < 0 then mod + n else mod

extension (self: Long)
  inline def >=<(n: Long): Boolean = self >= 0 && self < n
  inline def %%(n: Long): Long     =
    val mod = self % n
    if mod < 0 then mod + n else mod

// iterator extensions

extension [A](self: Iterator[A]) def findMap[B](f: A => Option[B]): B = self.flatMap(f).next()

private val WordRe = "\\S+".r

// string extensions

extension (self: String)
  def numbers: Vector[Long]          = NumRe.findAllIn(self).map(_.toLong).toVector
  def words: Vector[String]          = WordRe.findAllIn(self).toVector
  def commaSeparated: Vector[String] = self.split(',').toVector
  def characters: Vector[String]     = self.split("").toVector

// vector extensions

extension [A](self: Vector[A])
  def mapToMap[B, C](f: A => (B, C)): Map[B, C] = self.map(f).toMap

  // maps a vector with an accumulator, returning the final accumulator and values
  def mapAcc[B, C](c0: C)(f: (C, A) => (C, B)): (C, Vector[B]) =
    self.foldLeft(c0 -> Vector.empty[B]):
      case ((c, bs), a) => f(c, a) match { case (c2, b) => (c2, bs :+ b) }

  // stateful map, maps a vector with an accumulator then drops the accumulator at the end
  def mapS[B, C](c0: C)(f: (C, A) => (C, B)): Vector[B] = mapAcc(c0)(f)._2

// range extensions
extension (self: NumericRange[Long])
  def splitLess(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((limit - self.head).toInt)

  def splitGreater(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((1 + limit - self.head).toInt).swap

  def range: Long = if (self.isEmpty) 0 else 1 + self.last - self.head

// a board is a vector of strings

type Board = Vector[String]

extension (self: Board)
  def width                       = self.head.length
  def height                      = self.length
  def nw: Loc                     = Origin
  def se: Loc                     = Loc(width - 1, height - 1)
  def apply(loc: Loc): Char       = self(loc.y.toInt)(loc.x.toInt)
  def get(loc: Loc): Option[Char] = Option.when(loc >=< self)(self(loc))

  def find(char: Char): Loc = locations.find(apply(_) == char).get

  def locations: Vector[Loc] =
    self.indices.toVector.flatMap(y => self.head.indices.map(x => Loc(x, y)))

  def split: (Board, Board) =
    val index = self.indexWhere(_.isEmpty)
    self.slice(0, index) -> self.slice(1 + index, self.length)

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

  def adjacents: Vector[Loc] = Dir.values.map(this + _).toVector

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)

val Origin: Loc = Loc(0, 0)

// a direction and magnitude

final case class Vec(direction: Dir, magnitude: Long):
  def dx: Long = direction.dx * magnitude
  def dy: Long = direction.dy * magnitude

  inline def +(addend: Long): Vec     = copy(magnitude = magnitude + addend)
  inline def -(subtrahend: Long): Vec = copy(magnitude = magnitude - subtrahend)
  inline def *(multiplier: Long): Vec = copy(magnitude = magnitude * multiplier)

// geometries

extension (self: Vector[Vec])
  def vertices: Vector[Loc] = self.scanLeft(Origin)(_ + _)
  def perimeter: Long       = self.map(_.magnitude).sum

extension (self: Vector[Loc]) def area: Long = self.zip(self.tail).map((a, b) => a.x * b.y - b.x * a.y).sum.abs / 2
