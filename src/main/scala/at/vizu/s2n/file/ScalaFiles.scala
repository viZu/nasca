package at.vizu.s2n.file

import java.nio.file.{Files, Path, Paths}

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
      manipulateFileContent(source)
    } finally {
      source.close()
    }
  }

  def readFileRaw(path: Path): Array[Byte] = {
    Files.readAllBytes(path)
  }

  def checkFile(path: Path): Unit = {
    if (!Files.exists(path)) {
      throw new ArgumentException(s"Could not find file '${path.toString}'")
    } else if (Files.isDirectory(path)) {
      throw new ArgumentException(s"It was expected that '${path.toString}' is a file, but it was a directory")
    }
  }

  val pkgRegex = """^(package) +(.*) *;""".r

  def manipulateFileContent(source: Source): String = {
    val lines: List[String] = source.getLines().toList
    val line1: String = lines.head

    line1 match {
      case pkgRegex(_, pkg) => handlePackage(lines.tail, pkg)
      case _ => lines.mkString("\n")
    }
  }

  def handlePackage(lines: List[String], pkgString: String): String = {
    val prepend = new StringBuilder
    val append = new StringBuilder
    pkgString.split('.').foreach(pkg => {
      prepend.append("package " + pkg + " {")
      append.append('}')
    })
    val newList = prepend.toString() :: lines ::: List(append.toString())
    val s: String = newList.mkString("\n")
    s
  }

  def createDirectory(directory: Path) = {
    if (!Files.exists(directory)) {
      Files.createDirectories(directory)
    } else if (!Files.isDirectory(directory)) {
      throw new RuntimeException(s"File $directory was no directory")
    }
  }

  def deleteDirectory(directory: Path) = {
    val path = scalax.file.Path(directory.toFile)
    path.deleteRecursively(continueOnFailure = true)
  }

  def writeToFile(directory: Path, fileName: String, content: String): Unit = {
    writeToFile(directory, fileName, content.getBytes("utf-8"))
  }

  def writeToFile(directory: Path, fileName: String, content: Array[Byte]): Unit = {
    val path: Path = Paths.get(directory.toString, fileName)
    Files.write(path, content)
  }
}
