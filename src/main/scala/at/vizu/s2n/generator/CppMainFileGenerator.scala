package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.log.Debug
import at.vizu.s2n.log.Profiler._
import at.vizu.s2n.types.result.{Implementation, ObjectImplementation}
import at.vizu.s2n.types.symbol.TScope
import com.typesafe.scalalogging.LazyLogging

/**
  * Phil on 02.12.15.
  */
class CppMainFileGenerator(scope: TScope, impl: Implementation) extends MainFileGenerator with LazyLogging {

  private val tpe = impl.tpe

  override def generateMainFile(args: Arguments): Unit = {
    profileFunc(logger, "Generate main file", () => {
      checkMainClass()
      generateInternal(args)
    }, Debug)
  }

  private def generateInternal(args: Arguments) = {
    val content: String = generateContent()
    val prettyContent = CodePrettifier.prettify(content)
    writeFile(args, prettyContent)
  }

  private def generateContent(): String = {
    s"""#include "${GeneratorUtils.getHeaderFileName(tpe)}"
        |#include <iostream>
        |
       |int main(int argc, char **argv) {
        |  ${GeneratorUtils.getCppTypeName(tpe.pkg, tpe.simpleName, "")}::getInstance()->main();
        |  return 0;
        |}
     """.stripMargin
  }

  private def writeFile(args: Arguments, content: String) = {
    ScalaFiles.writeToFile(args.out, "main.cpp", content)
  }

  private def checkMainClass() = {
    impl match {
      case o: ObjectImplementation =>
        o.tpe.methods.find(_.name == "main") match {
          case Some(_) =>
          case None => throw new GeneratorException(s"Class ${impl.tpe.simpleName} does not have a main method")
        }
      case _ =>
        throw new GeneratorException(s"Class ${impl.tpe.simpleName} was expected to be an object, but was a class")
    }
  }

}
