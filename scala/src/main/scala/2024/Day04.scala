package `2024`

import zio.ZIOAppDefault
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import utils.readDayInput
import zio.stream.ZStream
import zio.Chunk

object Day04 extends ZIOAppDefault {

  override def run = for {
    input <- readDayInput(2024, 4)
      .map(parseLine)
      .runCollect

    part1 <- searchDirections(input).runCount
    _ <- zio.Console.printLine(s"Part1: $part1")

    part2 <- searchXMas(input).runCount
    _ <- zio.Console.printLine(s"Part2: $part2")

  } yield ()

  def parseLine(line: String): List[Char] = {
    line.toCharArray.toList
  }

  def searchXMas(grid: Chunk[List[Char]]) = {
    for {
      y <- ZStream.range(0, grid.length)
      x <- ZStream.range(0, grid(y).length)
      if grid.isXMas(x, y)
    } yield (x, y)
  }

  def searchDirections(
      grid: Chunk[List[Char]]
  ): ZStream[Any, Nothing, String] = {
    for {
      y <- ZStream.range(0, grid.length)
      x <- ZStream.range(0, grid(y).length)
      (dx, dy) <- ZStream.fromIterable(directions)
      res <- ZStream.fromZIO:
        ZStream
          .iterate((x, y)) { case (x1, y1) => (x1 + dx, y1 + dy) }
          .takeWhile(grid.has(_))
          .take("XMAS".size)
          .map(grid.get(_))
          .runFold("") { case (acc, c) => s"$acc$c" }
      if res == "XMAS"
    } yield res
  }

  def directions = for {
    i <- -1 to 1
    j <- -1 to 1
    if i != 0 || j != 0
  } yield (i, j)

  extension (grid: Chunk[List[Char]]) {
    def get(point: (Int, Int)) = grid(point._1)(point._2)

    def has(point: (Int, Int)) = point match {
      case (x, y) => y >= 0 && y < grid.length && x >= 0 && x < grid(y).length
    }

    def isMS(a: (Int, Int), b: (Int, Int)): Boolean = {
      val a1 = grid(a._2)(a._1)
      val b1 = grid(b._2)(b._1)

      a1 == 'M' && b1 == 'S' || a1 == 'S' && b1 == 'M'
    }
    def isXMas(x: Int, y: Int): Boolean =
      (y > 0 && y < (grid.length - 1))
        && (x > 0 && x < (grid(y).length - 1))
        && grid(y)(x) == 'A'
        && grid.isMS((x - 1, y - 1), (x + 1, y + 1))
        && grid.isMS((x - 1, y + 1), (x + 1, y - 1))

  }
}
