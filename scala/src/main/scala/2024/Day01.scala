package `2024`

import zio._
import utils.readDayInput
import zio.stream.ZStream

object Day01 extends ZIOAppDefault:
    def run = for {
        (left, right) <- readDayInput(2024, 1)
            .mapZIO(processLine)
            .runFold((List.empty[Long], List.empty[Long])) {
                case ((left, right), (l, r)) => (left :+ l, right :+ r)
            }
        distance <- ZIO.attempt:
            left.sorted
                .zip(right.sorted)
                .map(distance)
                .sum
        _ <- Console.printLine(s"Distance: $distance")
        similarity <- ZIO.attempt:
            left.map(similarity(right))
                .sum
        _ <- Console.printLine(s"Similarity: $similarity")
    } yield ()

    def distance(l: Long, r: Long): Long = Math.abs(r - l)
    def similarity(other: List[Long])(n: Long) = other.filter(_ == n).length * n

    private def processLine(line: String) = ZIO.attempt {
        val parts = line
            .split("\\s+")
            .map(_.toLong)
        if (parts.length != 2) 
            throw new RuntimeException(s"Invalid input: $line")
        else
            (parts(0), parts(1))
    }