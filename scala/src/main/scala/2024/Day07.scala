package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.ZIO
import zio.Chunk
import scala.annotation.tailrec

object Day07 extends ZIOAppDefault {

  def concatenation(a: Long, b: Long) = s"$a$b".toLong

  val OPERATIONS = List[(Long, Long) => Long](_ + _, _ * _)
  val OPERATIONS_V2 = List[(Long, Long) => Long](_ + _, _ * _, concatenation)

  val input = readDayInput(2024, 7)
  override def run = for {
    expressions <- input.mapZIO(parseLine(_)).runCollect
    part1 <- solution(OPERATIONS)(expressions)
    _ <- ZIO.debug(s"Part 1: $part1")
    part2 <- solution(OPERATIONS_V2)(expressions)
    _ <- ZIO.debug(s"Part 2: $part2")
  } yield ()

  def solution(
      operations: Seq[(Long, Long) => Long]
  )(expressions: Chunk[Expression]) = ZIO.from {
    expressions
      .filter(solve(operations, _))
      .map(_.value)
      .sum
  }

  def solve(
      operations: Seq[(Long, Long) => Long],
      expression: Expression
  ): Boolean = {
    val nOperations = expression.params.length - 1

    permutations(operations.toList)(nOperations)
      .map(eval(expression.params, _))
      .filter(_ == expression.value)
      .nonEmpty
  }

  def permutations(
      operations: List[(Long, Long) => Long]
  )(n: Int): Seq[List[(Long, Long) => Long]] =
    if (n == 0)
    then Seq(operations)
    else
      for {
        op <- operations
        p <- permutations(operations)(n - 1)
      } yield op :: p

  @tailrec def eval(args: List[Long], ops: List[(Long, Long) => Long]): Long =
    args match {
      case Nil            => -1
      case result :: Nil  => result
      case a :: b :: tail => eval(ops.head(a, b) :: tail, ops.tail)
    }

  val ARGS_REGEX = """\d+""".r
  case class Expression(value: Long, params: List[Long])
  def parseLine(line: String) = ZIO.attempt {
    val parts = line.split(":")
    val value = parts(0).toLong
    val params = ARGS_REGEX
      .findAllIn(parts(1))
      .map(_.toLong)
      .toList
    Expression(value, params)
  }

}
