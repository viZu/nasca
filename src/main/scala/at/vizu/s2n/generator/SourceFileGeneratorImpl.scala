package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol.BaseTypes

/**
  * Phil on 12.11.15.
  */
class SourceFileGeneratorImpl(_baseTypes: BaseTypes, implementation: Implementation) extends SourceFileGenerator {

  def tpe = implementation.tpe

  override def generateSourceFiles(args: Arguments): Unit = {
    val name = GeneratorUtils.getSourceFileName(tpe)
    println("Generating source file " + name)
    val content = generateContent()

    println("Writing source file " + name)
    ScalaFiles.writeToFile(args.out, name, content)
  }

  private def generateContent(): String = {
    s"""#include "${GeneratorUtils.getHeaderFileName(tpe)}"""" + "\n"
  }
}
