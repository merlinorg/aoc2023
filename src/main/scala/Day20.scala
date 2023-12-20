package org.merlin.aoc2023

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day20 extends AoC:

  type ModuleName = String

  final case class Pulse(
    source: ModuleName,
    destination: ModuleName,
    value: Boolean,
  )

  object Pulse:
    final val ButtonPress = Pulse("button", "broadcaster", false)

  sealed trait Module:
    def name: ModuleName
    def destinations: Vector[ModuleName]
    def pulses(value: Boolean): Vector[Pulse] = destinations.map(Pulse(name, _, value))

  final case class PassThrough(
    name: ModuleName,
    destinations: Vector[ModuleName],
  ) extends Module

  final case class FlipFlop(
    name: ModuleName,
    destinations: Vector[ModuleName],
    state: Boolean,
  ) extends Module

  final case class Conjunction(
    name: ModuleName,
    destinations: Vector[ModuleName],
    state: Set[ModuleName],
  ) extends Module

  extension [A, B](self: Map[A, Vector[B]])
    inline def add(a: A, b: B): Map[A, Vector[B]] = self.updatedWith(a):
      case None         => Some(Vector(b))
      case Some(values) => Some(values :+ b)

  final case class Machine(
    modules: Map[ModuleName, Module],
    sources: Map[ModuleName, Vector[ModuleName]]
  ):
    inline def +(module: Module): Machine =
      copy(
        modules = modules.updated(module.name, module),
        sources = module.destinations.foldLeft(sources)(_.add(_, module.name))
      )

  object Machine:
    final val Initial = Machine(Map.empty, Map.empty)

    def parse(lines: Vector[String]): Machine =
      val modules = lines.map:
        case s"%$name -> $targets" => FlipFlop(name, targets.split(", ").toVector, false)
        case s"&$name -> $targets" => Conjunction(name, targets.split(", ").toVector, Set.empty)
        case s"$name -> $targets"  => PassThrough(name, targets.split(", ").toVector)
      modules.foldLeft(Initial)(_ + _)

  final case class MachineState(
    machine: Machine,
    presses: Long = 0,
    queue: Queue[Pulse] = Queue.empty,
  ):
    def nextState: MachineState = queue.dequeueOption match
      case None =>
        copy(presses = presses + 1, queue = Queue(Pulse.ButtonPress))

      case Some((Pulse(source, destination, value), tail)) =>
        machine.modules.get(destination) match
          case Some(passThrough: PassThrough) =>
            copy(queue = tail ++ passThrough.pulses(value))

          case Some(flipFlop: FlipFlop) if !value =>
            val flipFlop2 = flipFlop.copy(state = !flipFlop.state)
            copy(machine = machine + flipFlop2, queue = tail ++ flipFlop.pulses(flipFlop2.state))

          case Some(conjunction: Conjunction) =>
            val conjunction2 =
              conjunction.copy(state = if value then conjunction.state + source else conjunction.state - source)
            val active       = machine.sources(conjunction.name).forall(conjunction2.state.contains)
            copy(machine = machine + conjunction2, queue = tail ++ conjunction.pulses(!active))

          case _ =>
            copy(queue = tail)

  final case class Problem1State(
    lows: Long,
    highs: Long,
    complete: Boolean,
  ):
    inline def +(state: MachineState): Problem1State = state.queue.headOption.map(_.value) match
      case Some(false)                   => copy(lows = lows + 1)
      case Some(true)                    => copy(highs = highs + 1)
      case None if state.presses == 1000 => copy(complete = true)
      case None                          => this

    def result: Long = lows * highs

  object Problem1State:
    final val Initial = Problem1State(0, 0, false)

  override def part1(lines: Vector[String]): Long =
    val machine = Machine.parse(lines)
    Iterator
      .iterate(MachineState(machine))(_.nextState)
      .scanLeft(Problem1State.Initial)(_ + _)
      .find(_.complete)
      .get
      .result

  final case class Problem2State(
    cycles: Map[ModuleName, Long],
  ):
    inline def +(state: MachineState): Problem2State = state.queue.headOption match
      case Some(Pulse(_, dst, false)) if cycles.get(dst).contains(0L) =>
        copy(cycles = cycles + (dst -> state.presses))
      case _                                                          => this

    def complete: Boolean = cycles.values.forall(_ > 0)
    def result: Long      = lcm(cycles.values)

    private def lcm(list: Iterable[Long]): Long      = list.foldLeft(1L)((a, b) => b * a / gcd(a, b))
    @tailrec private def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)

  object Problem2State:
    def apply(names: Vector[ModuleName]): Problem2State =
      new Problem2State(names.map(_ -> 0L).toMap)

  def subgraphs(machine: Machine): Vector[ModuleName] =
    val terminal = machine.sources.keys.find(!machine.modules.contains(_)).get
    (machine.sources(terminal): @unchecked) match
      case Vector(source) =>
        (machine.modules(source): @unchecked) match
          case Conjunction(name, _, _) =>
            machine.sources(name)

  override def part2(lines: Vector[String]): Long =
    val machine = Machine.parse(lines)
    Iterator
      .iterate(MachineState(machine))(_.nextState)
      .scanLeft(Problem2State(subgraphs(machine)))(_ + _)
      .find(_.complete)
      .get
      .result
