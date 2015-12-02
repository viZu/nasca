package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.types.result.{Implementation, ObjectImplementation}
import at.vizu.s2n.types.symbol.TScope

/**
  * Phil on 02.12.15.
  */
class CppMainFileGenerator(scope: TScope, impl: Implementation) extends MainFileGenerator {

  private val tpe = impl.tpe

  override def generateMainFile(args: Arguments): Unit = {
    println("Generating main file...")
    checkMainClass()

    generateInternal(args)
  }

  private def generateInternal(args: Arguments) = {
    val content: String = generateContent()
    val prettyContent = CodePrettifier.prettify(content)
    writeFile(args, prettyContent)
  }

  private def generateContent(): String = {
    s"""#include "${GeneratorUtils.getHeaderFileName(tpe)}"
        |
       |int main(int argc, char **argv) {
        |  ${GeneratorUtils.getCppTypeName(tpe.pkg, tpe.simpleName)}::getInstance()->main();
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
