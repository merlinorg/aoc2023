package org.merlin.aoc2023

object Day6 extends AoC:

  override def a(lines: Vector[String]): Long =
    val times = NumRe.findAllIn(lines.head).map(_.toLong).toList
    val distances = NumRe.findAllIn(lines(1)).map(_.toLong).toList
    val td = times.zip(distances).map: (time, dist) =>
      (1L until time).count(press => (time - press) * press > dist)
    td.product
  end a

  override def b(lines: Vector[String]): Long =
    val time = NumRe.findAllIn(lines.head).mkString.toLong
    val dist = NumRe.findAllIn(lines(1)).mkString.toLong
    val t0 = ((time - Math.sqrt(time * time - 4 * dist)) / 2).toLong
    val t1 = ((time + Math.sqrt(time * time - 4 * dist)) / 2).toLong
    t1 - t0
  end b

end Day6
