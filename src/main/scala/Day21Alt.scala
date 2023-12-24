package org.merlin.aoc2023

import scalaz.*
import scalaz.Scalaz.*

object Day21Alt extends AoC:

  final case class Problem1FSM(
    board: Board,
    locations: Set[Loc]
  ):
    def nextState: Problem1FSM =
      val nextLocations = for
        loc <- locations
        adj <- loc.adjacents
        if adj >=< board && board(adj) != '#'
      yield adj
      copy(locations = nextLocations)

  end Problem1FSM

  override def part1(board: Board): Long =
    Iterator
      .iterate(Problem1FSM(board, Set(board.find('S'))))(_.nextState)
      .drop(if (board.length == 11) 6 else 64)
      .next()
      .locations
      .size

  final case class Problem2FSM(
    board: Board,
    start: Loc,
    index: Int,
    locations: Set[Loc],
    interior0: Set[Loc] = Set.empty,
    interior1: Set[Loc] = Set.empty,
  ):
    // We observe that the interior of the selection is fully occupied, alternating between two states, so
    // we don't need to recompute the interior we can record it for history and only compute the
    // update for the perimeter of the diamond. Because the state oscillates between two values we have to
    // record two alternating histories. The perimeter is a bit fuzzy so we keep a fairly large band
    // of values in the active computation. Note that this doesn't work for the sample input because it is
    // not as regular as the real input.
    def nextState: Problem2FSM =
      val cutoff         = index - 13
      val interior       = locations.filter(_.manhattan(start) <= cutoff - 1)
      val nextLocations2 = for
        loc <- locations
        adj <- loc.adjacents
        if adj.manhattan(start) >= cutoff
        if board(adj.y.toInt %% board.length)(adj.x.toInt %% board.length) != '#'
      yield adj
      copy(index = index + 1, locations = nextLocations2, interior0 = interior1 ++ interior, interior1 = interior0)

    def allLocations: Set[Loc] = locations ++ interior1

  end Problem2FSM

  // Considering space as a grid of the repeated boards, we hypothesize that the growing diamond will
  // consist of completely filled interior boards that oscillate between two states and then a fairly
  // small number of other board shapes that make up the perimeter of the diamond. The interior will
  // grow quadratically like the area of a square, the perimeter will grow linearly. So we want to iterate
  // the FSM until we have identified the quantity of each unique board type for three consecutive steps;
  // this is enough information to fit all the polynomials and  solve. In fact we only need to identify the
  // unique boards by the number of active locations in them. it takes a couple of iterations before the
  // final set of possible shapes resolves.
  final case class Solution2FSM(
    steps: Long,
    size: Long,
    iteration: Long,
    countHistories: Map[Long, Vector[Long]]
  ):
    private val quotient = steps / size
    private val modulus  = steps % size

    inline def +(fsm: Problem2FSM): Solution2FSM =
      // only consider the even steps because the odd ones will have different shapes as we oscillate
      // between odd and even tiles
      if fsm.index % (size * 2) == modulus then
        val locations    = fsm.allLocations
        // count the number of locations within each board
        val boardCounts  = locations.foldLeft(Map.empty[(Long, Long), Long]):
          case (map, Loc(x, y)) =>
            map.updatedWith((x / size) -> (y / size)):
              case None    => Some(1)
              case Some(n) => Some(n + 1)
        // count the number of instances of each board count
        val countCounts  = boardCounts.values.foldLeft(Map.empty[Long, Long]):
          case (map, count) =>
            map.updatedWith(count):
              case None    => Some(1)
              case Some(n) => Some(n + 1)
        // if the count counts has a different key set then we haven't yet reached the stable board
        // configurations so discard the prior; if it's the same then we can add the new counts to
        // the histories for our polynomial fit
        val newHistories =
          if countHistories.keySet == countCounts.keySet then
            countHistories.view.map((k, v) => k -> (v :+ countCounts(k))).toMap
          else countCounts.view.mapValues(Vector(_)).toMap
        copy(countHistories = newHistories, iteration = 1 + iteration)
      else this

    def solution: Option[Long] =
      Option.when(countHistories.values.exists(_.length == 3)):
        countHistories.toList.foldMap:
          case (count, values) =>
            // our indexes are over the even steps so we have to halve the quotient. we're also looking
            // at values several iterations in so we he have to subtract the iteration count.
            val q = quotient / 2 - iteration + 3
            if values(0) == values(1) then
              count * values(0)
            else if values(1) - values(0) == values(2) - values(1) then
              val solved = values(0) + q * (values(1) - values(0))
              count * solved
            else
              count * solve(q, values(0), values(1), values(2))

    end solution

    private def solve(x: Long, y0: Long, y1: Long, y2: Long): Long =
      y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

  end Solution2FSM

  private def part2Real(board: Board): Long =
    val start = board.find('S')
    Iterator
      .iterate(Problem2FSM(board, start, 0, Set(start)))(_.nextState)
      .scanLeft(Solution2FSM(26501365, board.size, 0, Map.empty))(_ + _)
      .findMap(_.solution)

  private def part2Pretend(board: Board): Long =
    val start = board.find('S')
    Iterator
      .iterate(Problem2FSM(board, board.find('S'), 0, Set(start)))(_.nextState)
      .tapEach: fsm =>
        if (fsm.index == 6 || fsm.index == 10 || fsm.index == 50 || fsm.index == 100)
          println(s"${fsm.index} -> ${fsm.allLocations.size}")
      .drop(500)
      .next()
      .locations
      .size

  override def part2(board: Board): Long =
    if board.length == 11 then part2Pretend(board) else part2Real(board)
