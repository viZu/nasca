package at.vizu.s2n.file

import java.nio.file.{Path, Paths}

import scala.io.{BufferedSource, Source}

/**
 * Phil on 21.09.15.
 */
object Files {

  def readFile(path: String): String = {
    val pathAbsolute: Path = Paths.get(path).toAbsolutePath
    assert(java.nio.file.Files.exists(pathAbsolute), s"File $path does not exists")
    val file = Source.fromFile(path, "utf-8")
    try {
      file.mkString
    } finally {
      file.close()
    }
  }

}
