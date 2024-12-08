package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.Chunk
import scala.util.matching.Regex
import zio.ZIO
import zio.Ref

object Day05 extends ZIOAppDefault {

  val input = readDayInput(2024, 5)

  override def run = for {
    part1 <- input.runCollect
      .flatMap(parseInput(_))
      .map(part1(_))
    _ <- ZIO.debug(s"Part 1: $part1")

    part2 <- input.runCollect
      .flatMap(parseInput(_))
      .map(part2(_))
    _ <- ZIO.debug(s"Part 2: $part2")

  } yield ()

  val RULE_REGEX = new Regex("""(\d+)\|(\d+)""")

  def part2(context: Context) = {
    val fixed = for {
      line <- context.updates
      if (!isOrdered(context, line))
    } yield fix(context, line)
    fixed.map(f => f(f.size / 2)).sum
  }

  def fix(context: Context, line: List[Int]): List[Int] = {
    var res = line
    var idx = 0
    while (idx < line.size) {
      val toCheck = res(idx)
      val fix = for {
        previous <- context.rules.getOrElse(toCheck, Set.empty).toSeq
        previousIdx <- Some(res.indexOf(previous))
        if previousIdx > idx
      } yield (previousIdx, previous)
      fix.minByOption(_._1) match {
        case Some((previousIdx, previous)) => {
          res = res.updated(previousIdx, toCheck).updated(idx, previous)
        }
        case None => {
          idx += 1
        }
      }
    }
    res
  }

  def isOrdered(context: Context, line: List[Int]): Boolean = {
    val present = line.toSet
    def _isOrdered(seen: Set[Int], updates: List[Int]): Boolean =
      updates match {
        case Nil => true
        case x :: xs => {
          context.rules
            .getOrElse(x, Set.empty)
            .filter(present.contains(_))
            .map(seen.contains(_))
            .fold(true) { _ && _ }
          && _isOrdered(seen + x, xs)
        }
      }
    _isOrdered(Set.empty, line)
  }
  def part1(context: Context) = {
    val results = for {
      update <- context.updates
      if isOrdered(context, update)
    } yield update(update.size / 2)

    results.sum
  }

  def parseInput(lines: Chunk[String]) = ZIO.attempt {
    val init = ("rule", Map.empty[Int, Set[Int]], List.empty[List[Int]])
    val (_, rules, updates) = lines.foldLeft(init) {
      case (("rule", rules, _), line) =>
        RULE_REGEX
          .findFirstMatchIn(line)
          .map { m =>
            val prev = m.group(1).toInt
            val suc = m.group(2).toInt
            val rules1 = rules.updatedWith(suc) {
              case Some(set) => Some(set + prev)
              case None      => Some(Set(prev))
            }
            ("rule", rules1, List.empty)
          }
          .getOrElse(("update", rules, List.empty))
      case (("update", rules, updates), line) => {
        val parsed = line
          .split(",")
          .filter(_.nonEmpty)
          .map(_.toInt)
          .toList
        ("update", rules, updates :+ parsed)
      }
    }
    Context(rules, updates)
  }
  case class Context(
      val rules: Map[Int, Set[Int]],
      val updates: List[List[Int]]
  )

}
