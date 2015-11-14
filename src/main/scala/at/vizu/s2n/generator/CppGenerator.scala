package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.result.{ClassImplementation, ObjectImplementation, ScalaFileWrapper}
import at.vizu.s2n.types.symbol.BaseTypes

/**
 * Phil on 06.11.15.
 */
class CppGenerator(baseTypes: BaseTypes) extends Generator {

  override def generateCode(args: Arguments, fileContents: Seq[ScalaFileWrapper]): Unit = {

    println("Generating header files...")
    generateHeaders(args, fileContents)

    println("Generating source files...")
    generateSourceFiles(args, fileContents)
    ???
  }

  def generateHeaders(args: Arguments, fileContents: Seq[ScalaFileWrapper]) = {
    val generators: Seq[HeaderFileGenerator] = getHeaderGenerators(fileContents)
    generators.foreach(_.generateHeaderFile(args))
  }

  def getHeaderGenerators(fileContents: Seq[ScalaFileWrapper]) = {
    fileContents.flatMap(c => {
      c.impls.map({
        case oi: ObjectImplementation => new ObjectHeaderFileGenerator(baseTypes, c.pkg, c.imports, oi)
        case ci: ClassImplementation => new ClassHeaderFileGenerator(baseTypes, c.pkg, c.imports, ci)
      })
    })
  }

  def generateSourceFiles(args: Arguments, fileContents: Seq[ScalaFileWrapper]) = {
    val generators = getSourceGenerators(fileContents)
    generators.foreach(_.generateSourceFiles(args))
  }

  def getSourceGenerators(fileContents: Seq[ScalaFileWrapper]) = {
    fileContents.flatMap(c => {
      c.impls.map(new SourceFileGeneratorImpl(baseTypes, _))
    })
  }


}
