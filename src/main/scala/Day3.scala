package org.merlin.aoc2023

object Day3 extends AoC {
  private val RunRe = "\\d+|[^.]".r

  private final case class Run(x: Int, y: Int, str: String) {
    def isNum: Boolean = str.forall(_.isDigit)

    def toInt: Int = str.toInt

    def isAdjacent(r: Run): Boolean =
      (r.y - y).abs <= 1 && (x + str.length >= r.x && x <= r.x + r.str.length)
  }

  private def parse(lines: Vector[String]): Vector[Run] = {
    for {
      (line, y) <- lines.zipWithIndex
      mtch      <- RunRe.findAllMatchIn(line)
    } yield Run(mtch.start, y, mtch.matched)
  }

  override def a(lines: Vector[String]): Int = {
    val runs  = parse(lines)
    val parts = runs.filter(run =>
      run.isNum && runs.exists(r => !r.isNum && r.isAdjacent(run))
    )
    parts.map(_.toInt).sum
  }

  override def b(lines: Vector[String]): Int = {
    val runs  = parse(lines)
    val gears = for {
      g  <- runs if g.str == "*"
      adj = runs.filter(r => r.isNum && r.isAdjacent(g))
      if adj.length == 2
    } yield adj.map(_.toInt).product
    gears.sum
  }
}
