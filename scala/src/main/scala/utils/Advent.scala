package utils

import zio.stream.ZStream
import zio.stream.ZPipeline
import java.nio.file.Paths
import java.io.ByteArrayInputStream
import zio.ZIO
import java.io.FileReader
import java.io.FileInputStream
import java.nio.charset.Charset
import java.io.IOException
import zio.ZIOAppDefault
import zio.Scope
import zio.ZIOAppArgs

def inputFileStream(year: Int, day: Int, modifier: String = "") = {
  val filename = s"day${String.format("%02d", day)}$modifier.txt"
  val filePath = Paths.get(s"./src/main/resources/$year/$filename").toFile()
  new FileInputStream(filePath)
}

def readDayInput(
    year: Int,
    day: Int,
    modifier: String = ""
): ZStream[Any, Throwable, String] = {
  ZStream
    .fromInputStream:
      inputFileStream(year, day, modifier)
    .via:
      ZPipeline.utf8Decode
    .via:
      ZPipeline.splitLines
}
