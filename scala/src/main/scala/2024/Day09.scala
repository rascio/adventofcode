package `2024`

import zio.ZIOAppDefault
import utils.readDayInput
import zio.Chunk
import zio.ZIO

object Day09 extends ZIOAppDefault {

  override def run = for {
    input <- readDayInput(2024, 9).runCollect
    blocks <- parseInput(input)
    _ <- ZIO.debug(s"Part 1: ${part1(blocks)}") // 6154342787400
    _ <- ZIO.debug(s"Part 2: ${part2(blocks)}")
  } yield ()

  def part1(blocks: List[Block]): String = {
    blocks
      .compact()
      .checksum()
  }
  def part2(blocks: List[Block]): String = {
    blocks
      .defrag()
      .checksum()
  }

  class Defrag(blocks: List[Block]) {
    var fileIdx: Int = blocks.findLastFileBlock(blocks.size - 1)
    var file: FileBlock = blocks(fileIdx).asInstanceOf[FileBlock]
    var freeIdx: Int = blocks.findFirstFreeBlock(0)
    var free: FreeBlock = blocks(freeIdx).asInstanceOf[FreeBlock]
    var b = blocks

    def apply() = {
      while (file.id > 0) {
        if (findNextFree()) {
          val (left, right) = compactBlocks(free, file)
          b = b.patch(freeIdx, left, 1).patch(fileIdx + left.size - 1, right, 1)
        }
        findNextId()
      }
      b
    }

    def findNextId() = {
      LazyList
        .iterate(fileIdx - 1)(_ - 1)
        .filter(b(_).isInstanceOf[FileBlock])
        .map(i => (b(i).asInstanceOf[FileBlock], i))
        .find { case (fb, _) => fb.id < file.id }
        .map((fb, idx) => {
          fileIdx = idx
          file = fb
        })
        .getOrElse {
          fileIdx = -1
          file = null
        }
    }

    private val cache = collection.mutable.Map.empty[Int, Int]
    def findNextFree() = {
      LazyList
        .iterate(cache.getOrElse(file.size, 0))(_ + 1)
        .takeWhile(i => i < b.size && i < fileIdx)
        .filter(b(_).isInstanceOf[FreeBlock])
        .map(i => (b(i).asInstanceOf[FreeBlock], i))
        .find { case (fb, idx) =>
          fb.size >= file.size
        }
        .map((fb, idx) => {
          freeIdx = idx
          free = fb
          cache.put(file.size, idx + 1)
        })
        .isDefined
    }
  }

  extension (blocks: List[Block]) {

    def checksum(): String = {
      blocks.toSeq
        .foldLeft((0, BigInt(0))) {
          case ((idx, res), FreeBlock(size)) =>
            (idx + size, res)
          case ((idx, res), FileBlock(id, size)) => {
            val sizes =
              Seq.range(idx, idx + size).foldLeft(BigInt(0))(_ + _)
            (idx + size, res + (id * sizes))

          }
        }
        ._2
        .toString
    }
    def defrag() = {
      Defrag(blocks)()
    }
    def compact() = {
      var b = blocks
      var s = b.findLastFileBlock(b.size - 1)
      var f = b.findFirstFreeBlock(0)
      while (s > f) {
        val (left, right) =
          compactBlocks(
            b(f).asInstanceOf[FreeBlock],
            b(s).asInstanceOf[FileBlock]
          )
        b = b.patch(f, left, 1).patch(s + left.size - 1, right, 1)
        s = b.findLastFileBlock(s + left.size - 1)
        f = b.findFirstFreeBlock(f)
      }
      b
    }
    def findLastFileBlock(from: Int): Int = {
      var start = from
      while (start > 0) {
        if (blocks(start).isInstanceOf[FileBlock])
        then return start
        else start -= 1
      }
      return 0
    }
    def findFirstFreeBlock(from: Int): Int = {
      var start = from
      while (start < blocks.size) {
        if (blocks(start).isInstanceOf[FreeBlock])
        then return start
        else start += 1
      }
      return blocks.size
    }
    def debug() = {
      blocks
        .map {
          case FileBlock(id, size) => id.toString.repeat(size)
          case FreeBlock(size)     => ".".repeat(size)
        }
        .fold("")(_ + _)
    }
  }

  def compactBlocks(free: FreeBlock, file: FileBlock) = {
    if (free.size > file.size) {
      val left = Seq(
        FileBlock(file.id, file.size),
        FreeBlock(free.size - file.size)
      )
      val right = Seq(FreeBlock(file.size))
      (left, right)
    } else if (free.size < file.size) {
      val left = Seq(FileBlock(file.id, free.size))
      val right = Seq(
        FileBlock(file.id, file.size - free.size),
        FreeBlock(free.size)
      )
      (left, right)
    } else {
      val left = Seq(FileBlock(file.id, free.size))
      val right = Seq(FreeBlock(free.size))
      (left, right)
    }
  }

  sealed trait Block {
    val size: Int
  }
  case class FileBlock(id: Int, size: Int) extends Block
  case class FreeBlock(size: Int) extends Block
  def parseInput(input: Chunk[String]) = ZIO.attempt {
    input(0).toCharArray.zipWithIndex.map { case (c, i) =>
      if (i % 2 == 0)
      then FileBlock(i / 2, c.toString.toInt)
      else FreeBlock(c.toString.toInt)
    }.toList
  }

}
