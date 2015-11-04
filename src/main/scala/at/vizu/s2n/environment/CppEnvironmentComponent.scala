package at.vizu.s2n.environment

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.GeneratorComponent
import at.vizu.s2n.parser.{AST, ParserComponent}
import at.vizu.s2n.types.TypeSystemComponent

/**
 * Phil on 25.09.15.
 */
trait CppEnvironmentComponent extends EnvironmentComponent {
  this: ParserComponent with TypeSystemComponent with GeneratorComponent =>

  class CppEnvironment extends Environment {
    override def compile(args: Arguments): Unit = {
      val contents: Seq[(String, String)] = readFileContents(args.files)
      val trees: Seq[AST] = parser.parseContents(contents)
      typeSystem.checkTrees(trees)
    }

    private def readFileContents(files: Seq[Path]) = {
      val pathStrings: Seq[String] = files.map(_.toString)
      println(s"Reading file contents: $pathStrings")
      pathStrings.zip(ScalaFiles.readFiles(files))
    }

  }
}
