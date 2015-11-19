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
    val generatorPairs = getGeneratorTuples(scope, fileContents)
    generatorPairs.foreach(tuple => invokeGenerators(args, tuple._1, tuple._2))

    //    println("Generating header files...")
    //    generateHeaders(args, fileContents)
    //
    //    println("Generating source files...")
    //    generateSourceFiles(args, scope, fileContents)
    ???
  }


  def invokeGenerators(args: Arguments, sourceGenerator: SourceFileGenerator, headerGenerator: HeaderFileGenerator) = {
    val handles = sourceGenerator.generateSourceFile(args)
    headerGenerator.generateHeaderFile(args, handles)
  }

  def getGeneratorTuples(scope: TScope, fileContents: Seq[ScalaFileWrapper]): Seq[GeneratorPair] = {
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
    new SourceFileGeneratorImpl(baseTypes, classScope, impl)
  }

  //  def generateHeaders(args: Arguments, fileContents: Seq[ScalaFileWrapper]) = {
  //    val generators: Seq[HeaderFileGenerator] = getHeaderGenerators(fileContents)
  //    generators.foreach(_.generateHeaderFile(args))
  //  }
  //
  //  def getHeaderGenerators(fileContents: Seq[ScalaFileWrapper]) = {
  //    fileContents.flatMap(c => {
  //      c.impls.map({
  //        case oi: ObjectImplementation => new ObjectHeaderFileGenerator(baseTypes, c.pkg, c.imports, oi)
  //        case ci: ClassImplementation => new ClassHeaderFileGenerator(baseTypes, c.pkg, c.imports, ci)
  //      })
  //    })
  //  }
  //
  //  def generateSourceFiles(args: Arguments, scope: TScope, fileContents: Seq[ScalaFileWrapper]) = {
  //    val generators = getSourceGenerators(scope, fileContents)
  //    generators.foreach(_.generateSourceFile(args))
  //  }
  //
  //  def getSourceGenerators(scope: TScope, fileContents: Seq[ScalaFileWrapper]) = {
  //    fileContents.flatMap(c => {
  //      c.impls.map(i => {
  //        val classScope: TScope = scope.enterScope(i.tpe)
  //        c.imports.foreach(imp => {
  //          classScope.addTypeAlias(imp.rename, imp.pkg + "." + imp.name)
  //        })
  //        new SourceFileGeneratorImpl(baseTypes, classScope, i)
  //      })
  //    })
  //  }


}
