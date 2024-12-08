package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import scala.util.matching.Regex
import zio.stream.ZStream
import zio.ZIO
import java.util.Scanner
import utils.inputFileStream
import `2024`.Day03.Instruction
import zio.Scope
import zio.ZIOAppArgs

object Day03 extends ZIOAppDefault {
  val REGEX = Regex("""(mul|do|don't)\(((\d+,?)*?)\)""")

  val input = readDayInput(2024, 3).flatMap(processLine)

  def run = for {
    part1 <- input
      .runFold(0) {
        case (acc, Mult(a, b)) => acc + (a * b)
        case (acc, _)          => acc
      }
    _ <- zio.Console.printLine(s"Part1: $part1")
    (_, part2) <- input
      .runFold((true, 0)) {
        case ((active, sum), Mult(a, b)) if active =>
          (active, sum + (a * b))
        case ((active, sum), Do)   => (true, sum)
        case ((active, sum), Dont) => (false, sum)
        case ((active, sum), _)    => (active, sum)
      }
    _ <- zio.Console.printLine(s"Part2: $part2")
  } yield ()

  def processLine(line: String) =
    ZStream
      .fromIterator(REGEX.findAllMatchIn(line))
      .mapZIO { v =>
        for {
          op <- ZIO.attempt(v.group(1))
          args <- ZIO.attempt(parseArguments(v.group(2)))
          result <- Instruction.parse(op, args)
        } yield result
      }

  def parseArguments(args: String): List[String] =
    args
      .split(",")
      .toSeq
      .filter(_.nonEmpty)
      .toList

  object Instruction:
    def parse(name: String, args: List[String]) = ZIO.attempt:
      (name, args) match {
        case ("mul", n :: m :: Nil) => Mult(n.toInt, m.toInt)
        case ("do", Nil)    => Do
        case ("don't", Nil) => Dont
        case _              => throw new IllegalArgumentException(s"Invalid instruction: $name [$args]")
      }

  sealed trait Instruction
  case class Mult(val n: Int, val m: Int) extends Instruction
  case object Do extends Instruction
  case object Dont extends Instruction
}
