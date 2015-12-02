package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.result._
import at.vizu.s2n.types.symbol.{BaseTypes, TScope}

/**
 * Phil on 06.11.15.
 */
class CppGenerator(baseTypes: BaseTypes) extends Generator {

  type GeneratorPair = (SourceFileGenerator, HeaderFileGenerator)

  override def generateCode(args: Arguments, scope: TScope, fileContents: Seq[ScalaFileWrapper]): Unit = {
    println("Generating C++ files...")
    val generatorPairs = getGeneratorPairs(scope, fileContents)
    generatorPairs.foreach(tuple => invokeGenerators(args, tuple._1, tuple._2))

    if (args.main.isNonEmpty) {
      val mainGenerator = getMainClassGenerator(args, scope, fileContents)
      mainGenerator.generateMainFile(args)
    }
    ???
  }


  def invokeGenerators(args: Arguments, sourceGenerator: SourceFileGenerator, headerGenerator: HeaderFileGenerator) = {
    val handles = sourceGenerator.generateSourceFile(args)
    headerGenerator.generateHeaderFile(args, handles)
  }

  def getGeneratorPairs(scope: TScope, fileContents: Seq[ScalaFileWrapper]): Seq[GeneratorPair] = {
    fileContents.flatMap(c => {
      c.impls.map({
        case oi: ObjectImplementation =>
          (getSourceGenerator(scope, c.pkg, c.imports, oi), getObjectHeaderGenerator(c.pkg, c.imports, oi))
        case ci: ClassImplementation =>
          (getSourceGenerator(scope, c.pkg, c.imports, ci), getClassHeaderGenerator(c.pkg, c.imports, ci))
      })
    })
  }

  def getObjectHeaderGenerator(pkg: String, imports: Seq[ImportStmt], oi: ObjectImplementation) = {
    new ObjectHeaderFileGenerator(baseTypes, pkg, imports, oi)
  }

  def getClassHeaderGenerator(pkg: String, imports: Seq[ImportStmt], ci: ClassImplementation) = {
    new ClassHeaderFileGenerator(baseTypes, pkg, imports, ci)
  }

  def getSourceGenerator(scope: TScope, pkg: String, imports: Seq[ImportStmt], impl: Implementation) = {
    val classScope: TScope = scope.enterScope(impl.tpe)
    imports.foreach(i => {
      classScope.addTypeAlias(i.rename, i.pkg + "." + i.name)
    })
    classScope.addTypeAlias(impl.tpe.simpleName, impl.tpe.name)
    new SourceFileGeneratorImpl(baseTypes, classScope, impl)
  }

  def getMainClassGenerator(args: Arguments, scope: TScope, fileContents: Seq[ScalaFileWrapper]): MainFileGenerator = {
    fileContents
      .flatMap(_.impls)
      .find(_.tpe.simpleName == args.main)
      .map(new CppMainFileGenerator(scope, _))
      .getOrElse(throw new GeneratorException(s"Class with name ${args.main} not found"))
  }
}
