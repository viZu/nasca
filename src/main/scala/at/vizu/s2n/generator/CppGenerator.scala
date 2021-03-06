package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.log.Debug
import at.vizu.s2n.log.ProfilerWithErrors._
import at.vizu.s2n.types.result._
import at.vizu.s2n.types.symbol.{BaseTypes, GenericType, TSymbolTable}
import com.typesafe.scalalogging.LazyLogging

/**
 * Phil on 06.11.15.
 */
class CppGenerator(baseTypes: BaseTypes) extends Generator with LazyLogging {

  type GeneratorTuple = (SourceFileGenerator, HeaderFileGenerator, Implementation)

  override def generateCode(args: Arguments, scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]): Unit = {
    profileFunc(logger, "Generate C++ sources", () => {
      createGeneratedDir(args)
      invokeGeneratorTuples(args, scope, fileContents)
      invokeMain(args, scope, fileContents)
      invokeTemplates(args, scope, fileContents)
    })
  }

  private def createGeneratedDir(args: Arguments) = {
    ScalaFiles.deleteDirectory(args.out)
    ScalaFiles.createDirectory(args.generatedDir)
  }

  def invokeMain(args: Arguments, scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]) = {
    if (args.binType.isExecutable) {
      val mainGenerator = getMainClassGenerator(args, scope, fileContents)
      mainGenerator.generateMainFile(args)
    }
  }

  def invokeTemplates(args: Arguments, scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]) = {
    val generators = getTemplateGenerators(args, scope, fileContents)
    generators.foreach(_.generateTemplateFile(args))
  }

  def getTemplateGenerators(args: Arguments, scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]) = {
    fileContents.flatMap(c => {
      c.impls.filter(_.tpe.isInstanceOf[GenericType]).map({
        case ci: ClassImplementation =>
          val classScope: TSymbolTable = scope.enterScope(ci.tpe)
          c.imports.foreach(i => {
            classScope.addTypeAlias(i.rename, i.pkg + "." + i.name)
          })
          classScope.addTypeAlias(ci.tpe.simpleName, ci.tpe.name)
          classScope.currentPackage = c.pkg
          new CppTemplateFileGenerator(baseTypes, classScope, ci)
      })
    })
  }

  def invokeGeneratorTuples(args: Arguments, scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]) = {
    val tuples = getGeneratorPairs(scope, fileContents)
    tuples.foreach(invoke(args, _))
  }

  def invoke(args: Arguments, tuple: GeneratorTuple) = {
    profileFunc(logger, s"Generate class ${tuple._3.tpe}", () => {
      invokeGeneratorTuple(args, tuple._1, tuple._2)
    }, Debug)
  }

  def invokeGeneratorTuple(args: Arguments, sourceGenerator: SourceFileGenerator, headerGenerator: HeaderFileGenerator) = {
    val handles = sourceGenerator.generateSourceFile(args)
    headerGenerator.generateHeaderFile(args, handles)
  }

  def getGeneratorPairs(scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]): Seq[GeneratorTuple] = {
    fileContents.flatMap(c => {
      c.impls.filter(!_.tpe.isInstanceOf[GenericType]).map({
        case oi: ObjectImplementation =>
          (getSourceGenerator(scope, c.pkg, c.imports, oi), getObjectHeaderGenerator(c.pkg, c.imports, oi), oi)
        case ci: ClassImplementation =>
          (getSourceGenerator(scope, c.pkg, c.imports, ci), getClassHeaderGenerator(c.pkg, c.imports, ci), ci)
      })
    })
  }

  def getObjectHeaderGenerator(pkg: String, imports: Seq[ImportStmt], oi: ObjectImplementation) = {
    new ObjectHeaderFileGenerator(baseTypes, pkg, imports, oi)
  }

  def getClassHeaderGenerator(pkg: String, imports: Seq[ImportStmt], ci: ClassImplementation) = {
    new ClassHeaderFileGenerator(baseTypes, pkg, imports, ci)
  }

  def getSourceGenerator(scope: TSymbolTable, pkg: String, imports: Seq[ImportStmt], impl: Implementation) = {
    val classScope: TSymbolTable = scope.enterScope(impl.tpe)
    imports.foreach(i => {
      classScope.addTypeAlias(i.rename, i.pkg + "." + i.name)
    })
    classScope.addTypeAlias(impl.tpe.simpleName, impl.tpe.name)
    classScope.currentPackage = pkg
    new CppSourceFileGenerator(baseTypes, classScope, impl)
  }

  def getMainClassGenerator(args: Arguments, scope: TSymbolTable, fileContents: Seq[ScalaFileWrapper]): MainFileGenerator = {
    fileContents
      .flatMap(_.impls)
      .find(_.tpe.simpleName == args.main)
      .map(new CppMainFileGenerator(scope, _))
      .getOrElse(throw new GeneratorException(s"Class with name ${args.main} not found"))
  }
}
