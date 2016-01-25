package at.vizu.s2n.args

import java.nio.file.{Path, Paths}

import at.vizu.s2n.Constants
import at.vizu.s2n.log.{LogLevel, Warn}

/**
 * Phil on 21.09.15.
 */
case class Arguments(env: String = "",
                     files: Seq[Path] = Vector(),
                     out: Path = Paths.get(""),
                     main: String = "",
                     binType: BinaryType = Executable,
                     binName: String = "binary",
                     logLevel: LogLevel = Warn,
                     stdout: Option[String] = None,
                     stderr: Option[String] = None,
                     libs: Seq[Path] = Vector()) {

  lazy val generatedDir = out.resolve(Constants.GeneratedDir)
}

sealed trait BinaryType {
  def isLibrary: Boolean

  def isExecutable: Boolean = !isLibrary
}

object BinaryType {

  def apply(value: String) = value match {
    case "lib" => Library
    case "exe" => Executable
  }

  def values = Set(Executable, Library)

  def stringValues = Set("lib", "exe")
}

case object Executable extends BinaryType {
  def isLibrary: Boolean = false
}

case object Library extends BinaryType {
  def isLibrary: Boolean = true
}