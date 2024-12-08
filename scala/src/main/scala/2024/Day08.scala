package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.Chunk
import zio.ZIO

object Day08 extends ZIOAppDefault {
  override def run = for {
    input <- readDayInput(2024, 8).runCollect
    context <- parseInput(input)
    _ <- ZIO.debug(s"Part 1: ${countAntinodes(part1)(context)}")
    _ <- ZIO.debug(s"Part 2: ${countAntinodes(part2)(context)}")
  } yield ()


  def countAntinodes(strategy: Context => Seq[Point])(ctx: Context): Int = {
    strategy(ctx).toSet.size
  }

  def part2(ctx: Context): Seq[Point] = {
    val antinodes = for {
      (c, points) <- ctx.antennas.toSeq
      a <- points
      b <- points
      if a != b
      antinode <- getAntinodesV2(a, b)
    } yield antinode

    antinodes.flatMap(_.takeWhile(ctx.isIn))
  }
  def getAntinodesV2(a: Point, b: Point): Seq[Seq[Point]] = {
    val (ax, ay) = a
    val (bx, by) = b
    val dx = ax - bx
    val dy = ay - by
    
    val aGen = LazyList.iterate(a) { case (x, y) => (x + dx, y + dy) }
    val bGen = LazyList.iterate(b) { case (x, y) => (x - dx, y - dy) }
    Seq(aGen, bGen)
  }

  def part1(ctx: Context): Seq[Point] = for {
    (c, points) <- ctx.antennas.toSeq
    a <- points
    b <- points
    if a != b
    antinode <- getAntinodesV1(a, b)
    if ctx.isIn(antinode)
  } yield antinode

  def getAntinodesV1(a: Point, b: Point): List[Point] = {
    val (ax, ay) = a
    val (bx, by) = b
    val dx = ax - bx
    val dy = ay - by
    (ax + dx, ay + dy) :: (bx - dx, by - dy) :: Nil
  }

  opaque type Point = (Int, Int)
  case class Context(
      val size: (Int, Int),
      val antennas: Map[Char, List[Point]]
  ) {
    def isIn(point: Point) = {
      val (x, y) = point
      x >= 0 && x < size._1 && y >= 0 && y < size._2
    }
  }
  def parseInput(lines: Chunk[String]) = ZIO.attempt {
    val antennas = for {
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
      if c != '.'
    } yield (x, y) -> c

    Context(
        size = (lines(0).length, lines.length),
        antennas = antennas.foldLeft(Map.empty[Char, List[Point]]) {
          case (acc, (p, c)) =>
            acc.updatedWith(c) {
              case None     => Some(List(p))
              case Some(xs) => Some(p :: xs)
            }
        }
    )
  }
}
