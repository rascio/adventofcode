package `2024`

import zio.ZIOAppDefault
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import utils.readDayInput
import zio.Chunk

object Day11 extends ZIOAppDefault {

  opaque type Stone = Long
  override def run = for {
    input <- readDayInput(2024, 11).runCollect
    stones <- parseInput(input)
    _ <- ZIO.debug(s"Part 1: ${part1(stones)}")
    _ <- ZIO.debug(s"Part 2: ${part2(stones)}")
  } yield ()

  def part1(stones: List[Stone]) = {
    stones.map(count(_, 25)).sum
  }

  def part2(stones: Seq[Stone]) = {
    stones.map(count(_, 75)).sum
  }

  val splitCache = collection.mutable.Map.empty[(Stone, Int), Long]

  def count(stone: Stone, times: Int): Long = {
    if times == 0
    then 1
    else if splitCache.contains((stone, times))
    then splitCache((stone, times))
    else {
      var res = blink(stone).map(count(_, times - 1)).sum
      splitCache.put((stone, times), res)
      res
    }
  }
  def blink(stone: Stone): List[Stone] = {
    if (stone == 0)
    then List(1)
    else {
      val s = stone.toString
      if (s.length() % 2 == 0)
      then {
        val (left, right) = s.splitAt(s.length() / 2)
        List(left.toInt, right.toInt)
      } else {
        List(stone * 2024)
      }
    }
  }

  val NUMBERS_REGEX = """\d+""".r
  def parseInput(input: Chunk[String]) = ZIO.attempt {
    NUMBERS_REGEX
      .findAllIn(input(0))
      .map(_.toLong)
      .toList
  }
}
