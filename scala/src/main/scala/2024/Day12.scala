package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.Chunk
import zio.ZIO

object Day12 extends ZIOAppDefault {
  override def run = for {
    input <- readDayInput(2024, 12).runCollect
    garden <- parseInput(input)
    _ <- ZIO.debug(s"Part 1: ${{ part1(garden) }}") // S > 1441254
  } yield ()

  def part1(garden: Iterable[Region]) = {
    garden.foreach(v => println(v.debug()))
    garden.map(r => r.area() * r.perimeter()).sum
  }

  class Garden(val garden: Array[Array[Char]]) {

    def collectRegion(x: Int, y: Int, res: Set[(Int, Int)]): Set[(Int, Int)] = {
      val neighbors = for {
        (nx, ny) <- Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
        if nx >= 0 && nx < garden(0).size && ny >= 0 && ny < garden.size
      } yield (nx, ny)

      neighbors.foldLeft(res + ((x, y))) { case (acc, (nx, ny)) =>
        if (garden(ny)(nx) == garden(y)(x) && !acc.contains((nx, ny)))
        then collectRegion(nx, ny, acc + ((nx, ny)))
        else acc
      }
    }
  }
  case class Plot(plotId: Char, x: Int, y: Int, freeSides: Int)
  class Region(val plots: Iterable[Plot]) {
    def area() = plots.size
    def perimeter() = plots.toSeq.map(_.freeSides).sum

    def debug() =
      s"region ${plots.head.plotId} area: ${area()} perimeter: ${perimeter()}"
  }
  def parseInput(input: Chunk[String]) = ZIO.attempt {
    val garden = input.map(_.toCharArray()).toArray

    val height = garden.size
    val width = garden(0).size

    def freeSides(x: Int, y: Int): Int = {
      val plotId = garden(y)(x)
      val sides = for {
        (nx, ny) <- Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      } yield
        if (nx >= 0 && nx < width && ny >= 0 && ny < height)
        then garden(ny)(nx)
        else null
      sides.count(_ != plotId)
    }

    val points = for {
      y <- 0 until height
      x <- 0 until width
    } yield (y, x)

    points.foldLeft(Set.empty[Region]) { case (regions, (y, x)) =>
      if (regions.exists(_.plots.exists(p => p.x == x && p.y == y)))
      then regions
      else {
        val plots = for {
          (px, py) <- Garden(garden).collectRegion(x, y, Set.empty)
          fs = freeSides(px, py)
        } yield Plot(garden(y)(x), px, py, fs)

        regions + Region(plots)
      }
    }
  }
}
