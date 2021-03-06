package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.expression.{Expression, PrimaryConstructorExpression, SecondaryConstructorExpression}
import at.vizu.s2n.generator.handles._
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol._
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.runtime.universe._

/**
  * Phil on 12.11.15.
  */
class CppSourceFileGenerator(_baseTypes: BaseTypes, classScope: TSymbolTable, implementation: Implementation)
  extends SourceFileGenerator with LazyLogging {

  lazy val classInitMethodName = "__init__class__" + implementation.tpe.simpleName

  def tpe = implementation.tpe

  def pkg = tpe.pkg

  def nameSpace = GeneratorUtils.getNameSpace(tpe.pkg)

  private val fileName: String = GeneratorUtils.getSourceFileName(tpe)

  override def generateSourceFile(args: Arguments): Set[GeneratorHandle] = {
    logger.debug("Generating source file " + fileName)
    val context = generateContent(classScope)

    logger.debug("Writing source file " + fileName)
    val prettyContent = CodePrettifier.prettify(context.value)
    ScalaFiles.writeToFile(args.generatedDir, fileName, prettyContent)
    context.handles
  }

  private def generateContent(scope: TSymbolTable): GeneratorContext = {
    val context = generateContentAcc(scope)
    context.enhance(
      s"""${GeneratorUtils.generateIncludeGuard(pkg, fileName)}
         |${generateIncludes(context.handles)}
         |
         |${context.value}
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

  private def generateContentAcc(scope: TSymbolTable): GeneratorContext = {
    val memberContext: GeneratorContext = generateTypeBody(scope)
    val preContent = if (memberContext.handles.nonEmpty) {
      memberContext.handles.filter(_.isInstanceOf[MethodHandle]).map(_.content).mkString("\n\n") + "\n\n"
    } else {
      ""
    }
    memberContext.enhance(preContent + memberContext.value)
  }

  private def generateTypeBody(scope: TSymbolTable): GeneratorContext = {
    val (memberTrees, constructorContent) = partitionTypeBody

    val initCtx = generateContructorInit(scope, constructorContent)

    val members = generateMembers(scope, initCtx, memberTrees)
    GeneratorUtils.mergeGeneratorContexts(members, "\n\n") // remove unhandled member and contexts
  }

  private def partitionTypeBody = implementation.tree.impl.body.partition({
    case v: ValOrDefDef => true
    case _ => false
  })

  private def generateMembers(scope: TSymbolTable, initCtx: GeneratorContext, memberTrees: Seq[Tree]) = {
    (memberTrees.map({
      case d: DefDef => generateMethod(scope, d, initCtx)
      case v: ValDef => generateField(scope, v)
    }) :+ initCtx).filter(_.isNonEmpty)
  }

  private def generateMethod(scope: TSymbolTable, d: DefDef, initCtx: GeneratorContext): GeneratorContext = {
    scope.scoped((s: TSymbolTable) => {
      TypeUtils.createAndAddGenericModifiers(s, d.tparams)
      val method: Method = TypeUtils.findMethodForDef(s, d)
      val ctx: GeneratorContext = if (method.isAbstract || tpe.isTrait && method.constructor) ""
      else if (method.constructor) generateConstructor(s, method, initCtx, d)
      else {
        TypeUtils.addParamsToScope(s, method.params)
        val classString = GeneratorUtils.generateClassTemplate(tpe)
        val templateString = classString + GeneratorUtils.generateMethodTemplate(method)
        generateMethod(s, d.rhs, method.returnType, method.name, method.params, templateString)
      }
      ctx
    }, MethodScope)
  }

  private def generateConstructor(scope: TSymbolTable, method: Method, initCtx: GeneratorContext,
                                  d: DefDef): GeneratorContext = method match {
    case c: Constructor if c.primary =>
      if (initCtx.isEmpty) PrimaryConstructorExpression(scope, c, "").content
      else PrimaryConstructorExpression(scope, c, classInitMethodName).content
    case c: Constructor =>
      scope.scoped((childScope: TSymbolTable) => {
        TypeUtils.addParamsToScope(scope, c.params)
        val block = Expression.wrapInBlock(d.rhs)
        val stats: List[Expression] = block.stats.tail.map(Expression(scope.baseTypes, scope, _))
        val primaryCallArgs = block.stats.head match {
          case Apply(i: Ident, args) if i.name.toString == TypeUtils.ConstructorName => Expression(scope, args)
        }
        SecondaryConstructorExpression(scope, c, primaryCallArgs, stats).content
      }, MethodScope)
  }

  private def generateField(scope: TSymbolTable, v: ValDef): GeneratorContext = {
    val field: Field = TypeUtils.findField(scope, v, tpe)
    generateFieldBody(scope, v.rhs, field)
  }

  private def generateFieldBody(scope: TSymbolTable, body: Tree, field: Field): GeneratorContext = {
    body match {
      case b: Block => generateInitMethod(scope, b, field.name, field.tpe).removeContent()
      case EmptyTree => GeneratorContext()
      case _ =>
        val expr: Expression = Expression(_baseTypes, scope, body)
        val expression: GeneratorContext = generateExpression(scope, expr, returnable = false)
        val handle: GeneratorHandle = FieldInitializerHandle(field.name, expression.value)
        GeneratorContext(handles = Set(handle))
    }
  }

  private def generateInitMethod(scope: TSymbolTable, b: Block, varName: String, varTpe: TType): GeneratorContext = {
    val initMethodName = "__init__" + varName.toUpperCase
    val initCall = initMethodName + "()"
    val initializerHandle = FieldInitializerHandle(varName, initCall)
    val privateMethodHandle = generateInitMethodHandle(initMethodName, varTpe)
    val generatedMethod = generateMethod(scope, b, varTpe, initMethodName)
    val methodHandle = MethodHandle(generatedMethod.value)
    generatedMethod.enhance(initMethodName, Set(initializerHandle, privateMethodHandle, methodHandle))
  }

  private def generateInitMethodHandle(methodName: String, fieldTpe: TType) = {
    val m: Method = Method(Context("", 0), methodName, fieldTpe, Vector(Private))
    MethodDefinitionHandle(m)
  }

  private def generateMethod(scope: TSymbolTable, rhs: Tree, returnType: TType, methodName: String,
                             params: Seq[Param] = Vector(), templateString: String = ""): GeneratorContext = {
    val cppMethodName = getMethodName(methodName)
    val rhsBlock: Block = Expression.wrapInBlock(rhs)
    val methodBody: GeneratorContext = Expression
      .getBaseBlockExpression(_baseTypes, scope, rhsBlock, returnType != _baseTypes.unit).content
    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, returnType)
    val paramsString = GeneratorUtils.generateParamsString(_baseTypes, params, withVars = true)
    val methodString: String = s"""$templateString$returnTypeString $cppMethodName($paramsString) ${methodBody.value}"""
    methodBody.enhance(methodString, paramsString.handles ++ returnTypeString.handles)
  }

  private def getMethodName(mName: String) = {
    GeneratorUtils.getCppTypeName(_baseTypes, tpe, withTypeName = false) + "::" + GeneratorUtils.prettifyMethod(mName)
  }

  private def generateExpression(scope: TSymbolTable, expression: Expression, returnable: Boolean): GeneratorContext = {
    val exprCtx: GeneratorContext = expression.content
    val content: String = if (returnable) "return " + exprCtx.value else exprCtx.value
    exprCtx.enhance(content)
  }

  private def generateContructorInit(scope: TSymbolTable, constructorContent: List[Tree]): GeneratorContext = {
    if (tpe.isTrait) GeneratorContext()
    else constructorContent match {
      case Nil => GeneratorContext() // no intitialization is done
      case _ =>
        val block: Block = Expression.wrapInBlock(constructorContent)
        generateConstructorInitBlock(scope, block)
    }
  }

  private def generateConstructorInitBlock(scope: TSymbolTable, body: Block) = {
    val mdHandle = generateInitMethodHandle(classInitMethodName, _baseTypes.unit)
    val templateString = GeneratorUtils.generateClassTemplate(tpe)
    val methodCtx = generateMethod(scope, body, _baseTypes.unit, classInitMethodName, templateString = templateString)

    methodCtx + mdHandle
  }

}
