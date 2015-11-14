package at.vizu.s2n.environment

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.Generator
import at.vizu.s2n.parser.{AST, Parser}
import at.vizu.s2n.types.TypeSystem
import at.vizu.s2n.types.result.ScalaFileWrapper

/**
 * Phil on 06.11.15.
 */
class CppEnvironment(parser: Parser, typeSystem: TypeSystem, generator: Generator) extends Environment {

  override def compile(args: Arguments): Unit = {
    val contents: Seq[(String, String)] = readFileContents(args.files)
    val trees: Seq[AST] = parser.parseContents(contents)
    val fileContents: Seq[ScalaFileWrapper] = typeSystem.checkTrees(trees)
    generator.generateCode(args, fileContents)
  }

  private def readFileContents(files: Seq[Path]) = {
    val pathStrings: Seq[String] = files.map(_.toString)
    println( s"""Reading file contents: ${pathStrings.mkString(",")}""")
    pathStrings.zip(ScalaFiles.readFiles(files))
  }

}
