package at.vizu.s2n.file

import java.io.File
import java.nio.file.{Path, Paths}

import at.vizu.s2n.exception.ArgumentException

import scala.io.{BufferedSource, Source}

/**
 * Phil on 21.09.15.
 */
object Files {
  def readFiles(files: Seq[File]): Seq[String] = {
   files.map(readFile)
  }

  def readFile(file: File): String = {
    checkFile(file)
    val source = Source.fromFile(file, "utf-8")
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  def checkFile(file: File): Unit = {
    if(!file.exists()) {
      throw new ArgumentException(s"File '${file.getAbsoluteFile.toString}' does not exist")
    }
  }
}
