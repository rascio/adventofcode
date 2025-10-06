package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.Chunk
import zio.ZIO

object Day10 extends ZIOAppDefault {
  override def run = for {
    input <- readDayInput(2024, 10).runCollect
    topography <- parseInput(input)
    _ <- ZIO.debug(s"Part 1: ${part1(topography)}")
    _ <- ZIO.debug(s"Part 2: ${part2(topography)}")
  } yield ()

  def part1(topography: Topography) = {
    def trails(from: (Int, Int)): Seq[(Int, Int)] = {
      val s = topography.get(from)

      if s == 9
      then Seq(from)
      else
        topography
          .neighbors(from)
          .flatMap(n => trails(n))
    }
    val trailheads = for {
      y <- topography.height
      x <- topography.width
      coord = (x, y)
      if topography.get(coord) == 0
      t = trails(coord).toSet
    } yield t

    trailheads.map(_.size).sum
  }

  def part2(topography: Topography) = {
    def trails(from: (Int, Int)): Seq[(Int, Int)] = {
      val s = topography.get(from)

      if s == 9
      then Seq(from)
      else
        topography
          .neighbors(from)
          .flatMap(n => trails(n))
    }
    val trailheads = for {
      y <- topography.height
      x <- topography.width
      coord = (x, y)
      if topography.get(coord) == 0
      t = trails(coord)
    } yield t

    trailheads.map(_.size).sum
  }

  class Topography(topography: Array[Array[Int]]) {

    val width = 0 until topography(0).size
    val height = 0 until topography.size

    def get(coord: (Int, Int)) = topography(coord._2)(coord._1)

    def neighbors(coord: (Int, Int)) = {
      val v = get(coord)
      val (x, y) = coord

      for {
        (nx, ny) <- Seq(
          (x - 1, y),
          (x + 1, y),
          (x, y - 1),
          (x, y + 1)
        )
        if width.contains(nx) && height.contains(ny)
        if topography(ny)(nx) == v + 1
      } yield (nx, ny)
    }

    def debug() = {
      val iy = height.iterator
      while (iy.hasNext) {
        val ix = width.iterator
        val y = iy.next()
        while (ix.hasNext) {
          val x = ix.next()
          print(topography(y)(x))
        }
        println()
      }
    }
  }

  def parseInput(input: Chunk[String]) = ZIO.attempt {
    Topography(input.map(s => s.toCharArray().map(_.toString.toInt)).toArray)
  }
}
