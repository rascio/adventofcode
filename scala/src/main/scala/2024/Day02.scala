package `2024`

import zio.Console
import zio.ZIOAppDefault
import utils.readDayInput
import zio.ZIO
import scala.annotation.tailrec

object Day02 extends ZIOAppDefault {

    val input = readDayInput(2024, 2).mapZIO(parseLine)

    def run = {
        for {
            part1 <- input
                .filter(isSafeV1)
                .runCount
            _ <- Console.printLine(s"Part1: $part1")
            part2 <- input
                .filter(isSafeV2)
                .runCount
            _ <- Console.printLine(s"Part2: $part2")
        } yield ()
    }

    private def isSafeV2(record: List[Int]): Boolean = {
        (0 until record.length)
            .map(i => record.take(i) ++ record.drop(i + 1))
            .filter(isSafeV1)
            .nonEmpty
    }

    private def isSafeV1(record: List[Int]): Boolean = {
        def offset(a: Int, b: Int) = {
            val v = b - a
            v >= 1 && v <= 3
        }
        @tailrec def check(list: List[Int], fn: (Int, Int) => Boolean): Boolean = list match {
            case Nil => true
            case x :: Nil => true
            case x :: y :: xs if fn(x, y) => check(y :: xs, fn)
            case _ => false
        }
        
        check(record, (a, b) => a < b && offset(a, b))
            || check(record, (a, b) => b < a && offset(b, a))
    }

    private def parseLine(line: String) = ZIO.attempt {
        line.split("\\s+")
            .toList
            .map(_.toInt)
    }
}