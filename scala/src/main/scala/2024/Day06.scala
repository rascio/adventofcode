package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.Chunk
import zio.ZIO
import scala.annotation.tailrec

object Day06 extends ZIOAppDefault {

  override def run = for {
    input <- readDayInput(2024, 6).runCollect
    (map, start) <- parseInput(input)
    part1 <- part1(map, start)
    _ <- ZIO.debug(s"Part 1: $part1")
    part2 <- part2(map, start)
    _ <- ZIO.debug(s"Part 2: $part2")
    // 1629 > S > 1485 && S != 1636 && S != 1587

  } yield ()

  def part1(map: Array[Array[Boolean]], start: (Int, Int)) = ZIO.attempt {
    findGuardPath(map, start).toSet.size
  }
  val TURN = -90
  def findGuardPath(map: Array[Array[Boolean]], start: (Int, Int)) = {
    @tailrec def solve(
        position: (Int, Int),
        direction: Int,
        visited: List[(Int, Int)]
    ): List[(Int, Int)] = {
      //   printMap(map, position, direction)
      if (!map.has(position))
      then visited
      else {
        val (pos, dir) = map.step(position, direction)
        solve(pos, dir, visited :+ position)
      }
    }
    // (4,6),
    solve(start, 180, List.empty)
  }

  def part2(map: Array[Array[Boolean]], start: (Int, Int)) = ZIO.attempt {
    val solution = findGuardPath(map, start)

    solution
      .filter { case (x, y) =>
        val map2 = map.map(_.clone)
        map2(y)(x) = true
        isInfinitePath(map2, start)
      }
      .toSet
      .size
  }

  def isInfinitePath(map: Array[Array[Boolean]], start: (Int, Int)) = {

    case class Visited(position: (Int, Int), direction: Int)
    @tailrec def solve(
        position: (Int, Int),
        direction: Int,
        visited: Set[Visited]
    ): Boolean = {
      if !map.has(position)
      then false
      else {
        val v = Visited(position, direction)
        visited.contains(v) || {
          val (pos, dir) = map.step(position, direction)
          solve(pos, dir, visited + v)
        }
      }
    }
    solve(start, 180, Set.empty)
  }

  def parseInput(input: Chunk[String]) = ZIO.attempt {
    val m = new Array[Array[Boolean]](input.size)
    var start = (0, 0)
    for (y <- 0 until input.size) {
      m(y) = new Array[Boolean](input(y).size)
      for (x <- 0 until input(y).size) {
        input(y)(x) match {
          case '#' => m(y)(x) = true
          case '.' => m(y)(x) = false
          case '^' => {
            start = (x, y)
            m(y)(x) = false
          }
        }
      }
    }
    (m, start)
  }

  extension (map: Array[Array[Boolean]]) {
    @tailrec def step(
        position: (Int, Int),
        direction: Int
    ): ((Int, Int), Int) = {
      val p = move(position, direction)
      if (map.has(p) && map(p._2)(p._1))
      then step(position, (direction + TURN) % 360)
      else (p, direction)
    }

    def has(position: (Int, Int)) = position match {
      case (x, y) => x >= 0 && y >= 0 && y < map.size && x < map(y).size
    }
  }

  def move(position: (Int, Int), direction: Int) =
    (
        position._1 + dir(Math.sin)(direction),
        position._2 + dir(Math.cos)(direction)
    )

  def dir(fn: Double => Double)(n: Int): Int =
    Math.round(fn(Math.toRadians(n))).toInt

  def printMap(
      map: Array[Array[Boolean]],
      position: (Int, Int),
      direction: Int
  ) = {
    for (y <- 0 until map.size) {
      for (x <- 0 until map(y).size) {
        val c =
          if (position == (x, y))
          then
            Math.abs(Math.round(direction / 90)) % 4 match {
              case 0 => "v"
              case 1 => ">"
              case 2 => "^"
              case 3 => "<"
            }
          else if (map(y)(x)) "#"
          else "."

        print(c)

      }
      println()
    }
  }
}
