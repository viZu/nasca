package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.expression.{ConstructorExpression, Expression}
import at.vizu.s2n.generator.handles._
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol._
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.runtime.universe._

/**
  * Phil on 12.11.15.
  */
class CppSourceFileGenerator(_baseTypes: BaseTypes, classScope: TScope, implementation: Implementation)
  extends SourceFileGenerator with LazyLogging {

  lazy val classInitMethodName = "__init__class__" + implementation.tpe.simpleName

  def tpe = implementation.tpe

  def pkg = tpe.pkg

  def nameSpace = GeneratorUtils.getNameSpace(tpe.pkg)

  private val fileName: String = GeneratorUtils.getSourceFileName(tpe)

  override def generateSourceFile(args: Arguments): Set[GeneratorHandle] = {
    logger.debug("Generating source file " + fileName)
    addGenericsToScope(classScope)
    val context = generateContent(classScope)

    logger.debug("Writing source file " + fileName)
    val prettyContent = CodePrettifier.prettify(context.content)
    ScalaFiles.writeToFile(args.generatedDir, fileName, prettyContent)
    context.handles
  }

  private def addGenericsToScope(scope: TScope) = {
    implementation.tpe match {
      case g: GenericType => g.genericModifiers.foreach(scope.addClass)
      case _ =>
    }
  }

  private def generateContent(scope: TScope): GeneratorContext = {
    val context = generateContentAcc(scope)
    context.enhance(
      s"""${GeneratorUtils.generateIncludeGuard(pkg, fileName)}
         |${generateIncludes(context.handles)}
         |
         |${context.content}
         |
         |${GeneratorUtils.generateEndIf(pkg, fileName)}""".stripMargin).removeHandles(classOf[IncludeHandle])
  }

  private def generateIncludes(handles: Set[GeneratorHandle]) = {
    val usedHeader: Set[String] = TypeUtils.getUsedTypes(_baseTypes, tpe).map(GeneratorUtils.getHeaderFileName)
    val includeHandles = handles.collect({ case ih: IncludeHandle => ih })
      .filter(handle => !usedHeader.contains(handle.cppInclude))
    val includeHeader = s"""#include "${GeneratorUtils.getHeaderFileName(tpe)}""""
    (includeHeader +: includeHandles.toSeq.map(_.content)).mkString("\n")
  }

  private def generateContentAcc(scope: TScope): GeneratorContext = {
    val memberContext: GeneratorContext = generateMember(scope)
    val preContent = if (memberContext.handles.nonEmpty) {
      memberContext.handles.filter(_.isInstanceOf[MethodHandle]).map(_.content).mkString("\n\n") + "\n\n"
    } else {
      ""
    }
    val templateCopyConstructors = GeneratorUtils.generateCopyConstructorsSource(_baseTypes, tpe)
    memberContext.enhance(preContent + memberContext.content + templateCopyConstructors)
  }

  private def generateMember(scope: TScope): GeneratorContext = {
    val (memberTrees, constructorContent) = implementation.tree.impl.body.partition({
      case v: ValOrDefDef => true
      case _ => false
    })

    val initCtx = generateContructorInit(scope, constructorContent)

    val members = (memberTrees.map({
      case d: DefDef => generateMethod(scope, d, initCtx)
      case v: ValDef => generateField(scope, v)
    }) :+ initCtx).filter(_.isNonEmpty)
    GeneratorUtils.mergeGeneratorContexts(members, "\n\n") // remove unhandled member and contexts
  }

  private def generateMethod(scope: TScope, d: DefDef, initCtx: GeneratorContext): GeneratorContext = {
    scoped(scope, (s: TScope) => {
      TypeUtils.createAndAddGenericModifiers(scope, d.tparams)
      val method: Method = TypeUtils.findMethodForDef(scope, d)
      if (method.isAbstract || tpe.isTrait && method.constructor) ""
      else if (method.constructor) generateConstructor(scope, method, initCtx)
      else {
        TypeUtils.addParamsToScope(s, method.params)
        val classString = GeneratorUtils.generateClassTemplate(tpe)
        val templateString = classString + GeneratorUtils.generateMethodTemplate(method)
        generateMethod(s, d.rhs, method.returnType, method.name, method.params, templateString)
      }
    })
  }

  private def generateConstructor(scope: TScope, method: Method, initCtx: GeneratorContext): GeneratorContext = {
    if (initCtx.isEmpty) ConstructorExpression(scope, method, "").generate // no in
    else ConstructorExpression(scope, method, classInitMethodName).generate
  }

  private def generateField(scope: TScope, v: ValDef): GeneratorContext = {
    val field: Field = TypeUtils.findField(scope, v, tpe)
    generateFieldBody(scope, v.rhs, field)
  }

  private def generateFieldBody(scope: TScope, body: Tree, field: Field): GeneratorContext = {
    body match {
      case b: Block => generateInitMethod(scope, b, field.name, field.tpe).removeContent()
      case EmptyTree => GeneratorContext()
      case _ =>
        val expr: Expression = Expression(_baseTypes, scope, body)
        val expression: GeneratorContext = generateExpression(scope, expr, returnable = false)
        val handle: GeneratorHandle = FieldInitializerHandle(field.name, expression.content)
        GeneratorContext(handles = Set(handle))
    }
  }

  private def generateInitMethod(scope: TScope, b: Block, varName: String, varTpe: TType): GeneratorContext = {
    val initMethodName = "__init__" + varName.toUpperCase
    val initCall = initMethodName + "()"
    val initializerHandle = FieldInitializerHandle(varName, initCall)
    val privateMethodHandle = generateInitMethodHandle(initMethodName, varTpe)
    val templateString = GeneratorUtils.generateClassTemplate(tpe)
    val generatedMethod = generateMethod(scope, b, varTpe, initMethodName, templateString = templateString)
    val methodHandle = MethodHandle(generatedMethod.content)
    generatedMethod.enhance(initMethodName, Set(initializerHandle, privateMethodHandle, methodHandle))
  }

  private def generateInitMethodHandle(methodName: String, fieldTpe: TType) = {
    val m: Method = Method(Context("", 0), methodName, fieldTpe, Vector(Private))
    MethodDefinitionHandle(m)
  }

  private def generateMethod(scope: TScope, rhs: Tree, returnType: TType, methodName: String,
                             params: Seq[Param] = Vector(), templateString: String = ""): GeneratorContext = {
    val cppMethodName = getMethodName(methodName)
    val rhsBlock: Block = Expression.wrapInBlock(rhs)
    val methodBody: GeneratorContext = Expression
      .getBaseBlockExpression(_baseTypes, scope, rhsBlock, returnType != _baseTypes.unit).generate
    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, returnType)
    val paramsString = GeneratorUtils.generateParamsString(_baseTypes, params, withVars = true)
    val methodString: String = s"""$templateString$returnTypeString $cppMethodName($paramsString) ${methodBody.content}"""
    methodBody.enhance(methodString, paramsString.handles)
  }

  private def getMethodName(mName: String) = {
    GeneratorUtils.getCppTypeName(_baseTypes, tpe, withTypeName = false) + "::" + GeneratorUtils.prettifyMethod(mName)
  }

  private def generateExpression(scope: TScope, expression: Expression, returnable: Boolean): GeneratorContext = {
    val exprCtx: GeneratorContext = expression.generate
    val content: String = if (returnable) "return " + exprCtx.content else exprCtx.content
    exprCtx.enhance(content)
  }

  private def generateContructorInit(scope: TScope, constructorContent: List[Tree]): GeneratorContext = {
    if (tpe.isTrait) GeneratorContext()
    else constructorContent match {
      case Nil => GeneratorContext() // no intitialization is done
      case _ =>
        val block: Block = Expression.wrapInBlock(constructorContent)
        generateConstructorInitBlock(scope, block)
    }
  }

  private def generateConstructorInitBlock(scope: TScope, body: Block) = {
    val mdHandle = generateInitMethodHandle(classInitMethodName, _baseTypes.unit)
    val templateString = GeneratorUtils.generateClassTemplate(tpe)
    val methodCtx = generateMethod(scope, body, _baseTypes.unit, classInitMethodName, templateString = templateString)
    val mHandle = MethodHandle(methodCtx.content)

    methodCtx.enhance("", Set(mHandle, mdHandle))
  }


  private def scoped(parentScope: TScope, f: TScope => GeneratorContext): GeneratorContext = {
    val childScope: TScope = parentScope.enterScope()
    val generated = f(childScope)
    childScope.exitScope()
    generated
  }

  // TODO many types of pathelements...


}
