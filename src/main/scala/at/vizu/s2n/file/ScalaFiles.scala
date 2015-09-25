package at.vizu.s2n.file

import java.nio.file.{Files, Path}

import at.vizu.s2n.exception.ArgumentException

import scala.io.Source

/**
 * Phil on 21.09.15.
 */
object ScalaFiles {
  def readFiles(files: Seq[Path]): Seq[String] = {
    files.map(readFile)
  }

  def readFile(path: Path): String = {
    checkFile(path)
    val source = Source.fromFile(path.toString, "utf-8")
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  def checkFile(path: Path): Unit = {
    if (Files.exists(path)) {
      throw new ArgumentException(s"File '${path.toString}' does not exist")
    }
  }
}
