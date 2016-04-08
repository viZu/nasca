package at.vizu.s2n.environment

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.ext.compiler.ExtCompiler
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.Generator
import at.vizu.s2n.log.Profiler
import at.vizu.s2n.packaging.Packager
import at.vizu.s2n.parser.{AST, Parser}
import at.vizu.s2n.types.TypeSystem
import com.typesafe.scalalogging.LazyLogging

/**
 * Phil on 06.11.15.
 */
class CppEnvironment(parser: Parser, typeSystem: TypeSystem, generator: Generator,
                     extCompiler: ExtCompiler, packager: Packager)
  extends Environment with LazyLogging {

  override def compile(args: Arguments): Unit = {
    Profiler.profileFunc(logger, "Success. Total time:", () => {
      val contents: Seq[(String, String)] = readFileContents(args.files)
      val trees: Seq[AST] = parser.parseContents(contents)
      val (scope, fileContents) = typeSystem.checkTrees(args, trees)
      generator.generateCode(args, scope, fileContents)
      extCompiler.compile(args)
      packager.packageBinary(args, scope)
    })
  }

  private def readFileContents(files: Seq[Path]) = {
    logger.debug(s"Reading file contents:")
    val pathStrings: Seq[String] = files.map(_.toString)
    pathStrings.foreach(s => logger.debug("    " + s))
    ScalaFiles.readFiles(files)
  }

}
