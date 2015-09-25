package at.vizu.s2n.environment

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.parser.{AST, ParserComponent}
import at.vizu.s2n.types.{TypeSystem, TypeSystemBuilderComponent}

/**
 * Phil on 25.09.15.
 */
trait CppEnvironmentComponent extends EnvironmentComponent {
  this: ParserComponent with TypeSystemBuilderComponent =>

  class CppEnvironment extends Environment {
    override def compile(args: Arguments): Unit = {
      val contents: Seq[(String, String)] = readFileContents(args.files)
      val trees: Seq[AST] = parser.parseContents(contents)

      val typeSystem: TypeSystem = typeSystemBuilder.build(trees)
    }

    private def readFileContents(files: Seq[Path]) = {
      files.map(_.toString).zip(ScalaFiles.readFiles(files))
    }

  }
}
