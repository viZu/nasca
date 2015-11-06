package at.vizu.s2n.environment

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.parser.{AST, Parser}
import at.vizu.s2n.types.TypeSystem

/**
 * Phil on 06.11.15.
 */
class CppEnvironment(parser: Parser, typeSystem: TypeSystem) extends Environment {

  override def compile(args: Arguments): Unit = {
    val contents: Seq[(String, String)] = readFileContents(args.files)
    if (contents.nonEmpty) {
      val trees: Seq[AST] = parser.parseContents(contents)
      typeSystem.checkTrees(trees)
    }
  }

  private def readFileContents(files: Seq[Path]) = {
    val pathStrings: Seq[String] = files.map(_.toString)
    println(s"Reading file contents: $pathStrings")
    pathStrings.zip(ScalaFiles.readFiles(files))
  }

}
