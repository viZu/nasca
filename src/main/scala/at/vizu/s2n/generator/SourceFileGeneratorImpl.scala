package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.expression.{ConstructorExpression, Expression}
import at.vizu.s2n.generator.handles._
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._

/**
  * Phil on 12.11.15.
  */
class SourceFileGeneratorImpl(_baseTypes: BaseTypes, classScope: TScope, implementation: Implementation) extends SourceFileGenerator {

  lazy val classInitMethodName = "__init__class__" + implementation.tpe.simpleName

  def tpe = implementation.tpe

  def nameSpace = GeneratorUtils.getNameSpace(tpe.pkg)

  override def generateSourceFile(args: Arguments): Seq[GeneratorHandle] = {
    val name = GeneratorUtils.getSourceFileName(tpe)
    println("Generating source file " + name)

    val context = generateContent(classScope)

    println("Writing source file " + name)
    val prettyContent = CodePrettifier.prettify(context.content)
    ScalaFiles.writeToFile(args.out, name, prettyContent)
    context.handles
  }

  private def generateContent(scope: TScope): GeneratorContext = {
    val context = generateContentAcc(scope)
    context.enhance(
      s"""${generateIncludes(context.handles)}
         |
         |${context.content}""".stripMargin).removeHandles(classOf[IncludeHandle])
  }

  private def generateIncludes(handles: Seq[GeneratorHandle]) = {
    val includeHandles = handles.collect({ case ih: IncludeHandle => ih }).distinct
    val includeHeader = s"""#include "${GeneratorUtils.getHeaderFileName(tpe)}""""
    (includeHeader +: includeHandles.map(_.content)).mkString("\n")
  }

  private def generateContentAcc(scope: TScope): GeneratorContext = {
    val memberContext: GeneratorContext = generateMember(scope)
    val preContent = if (memberContext.handles.nonEmpty) {
      memberContext.handles.filter(_.isInstanceOf[MethodHandle]).map(_.content).mkString("\n\n") + "\n\n"
    } else {
      ""
    }
    memberContext.enhance(preContent + memberContext.content)
  }

  private def generateMember(scope: TScope): GeneratorContext = {
    val (memberTrees, constructorContent) = implementation.tree.impl.body.partition({
      case v: ValOrDefDef => true
      case _ => false
    })

    val initCtx: GeneratorContext = generateContructorInit(scope, constructorContent)

    val members = (memberTrees.map({
      case d: DefDef => generateMethod(scope, d)
      case v: ValDef => generateField(scope, v)
    }) :+ initCtx).filter(_.isNonEmpty)
    GeneratorUtils.mergeGeneratorContexts(members, "\n\n") // remove unhandled member and contexts
  }

  private def generateMethod(scope: TScope, d: DefDef): GeneratorContext = {
    val method: Method = TypeUtils.findMethodForDef(scope, d)
    if (method.constructor) generateConstructor(scope, method)
    else
      scoped(scope, (s: TScope) => {
        TypeUtils.addParamsToScope(s, method.params)
        generateMethod(s, d.rhs, method.returnType, method.name, method.params)
      })
  }

  private def generateConstructor(scope: TScope, method: Method): GeneratorContext = {
    ConstructorExpression(_baseTypes, method, classInitMethodName).generate
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
        GeneratorContext(handles = Seq(handle))
    }
  }

  private def generateInitMethod(scope: TScope, b: Block, varName: String, varTpe: TType): GeneratorContext = {
    val initMethodName = "__init__" + varName.toUpperCase
    val initCall = initMethodName + "()"
    val initializerHandle = FieldInitializerHandle(varName, initCall)
    val privateMethodHandle = generateInitMethodHandle(initMethodName, varTpe)
    val generatedMethod = generateMethod(scope, b, varTpe, initMethodName)
    val methodHandle = MethodHandle(generatedMethod.content)
    generatedMethod.enhance(initMethodName, Seq(initializerHandle, privateMethodHandle, methodHandle))
  }

  private def generateInitMethodHandle(methodName: String, fieldTpe: TType) = {
    val m: Method = Method(Context("", 0), methodName, fieldTpe, Seq(Private))
    MethodDefinitionHandle(m)
  }

  private def generateMethod(scope: TScope, rhs: Tree, returnType: TType, methodName: String, params: Seq[Param] = Seq()): GeneratorContext = {
    val cppMethodName = getMethodName(methodName)
    val rhsBlock: Block = Expression.wrapInBlock(rhs)
    val methodBody: GeneratorContext = Expression.getBlockExpression(_baseTypes, scope, rhsBlock, returnType != _baseTypes.unit).generate //  generateMethodBody(scope, rhs, _baseTypes.unit != returnType)
    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, returnType)
    val paramsString = GeneratorUtils.generateParamsString(_baseTypes, params, withVars = true)
    val methodString: String = s"""$returnTypeString $cppMethodName($paramsString) ${methodBody.content}"""
    methodBody.enhance(methodString)
  }

  private def getMethodName(methodName: String) = GeneratorUtils.getCppTypeName(_baseTypes, tpe) + "::" + methodName

  private def generateExpression(scope: TScope, expression: Expression, returnable: Boolean): GeneratorContext = {
    val exprCtx: GeneratorContext = expression.generate
    val content: String = if (returnable) "return " + exprCtx.content else exprCtx.content
    exprCtx.enhance(content)
  }

  private def generateContructorInit(scope: TScope, constructorContent: List[Tree]): GeneratorContext = {
    constructorContent match {
      case Nil => generateConstructorInitBlock(scope, Expression.wrapInBlock(EmptyTree))
      case _ =>
        val block: Block = Expression.wrapInBlock(constructorContent)
        generateConstructorInitBlock(scope, block)
    }
  }

  private def generateConstructorInitBlock(scope: TScope, body: Block) = {
    val mdHandle = generateInitMethodHandle(classInitMethodName, _baseTypes.unit)
    val methodCtx = generateMethod(scope, body, _baseTypes.unit, classInitMethodName)
    val mHandle = MethodHandle(methodCtx.content)

    methodCtx.enhance("", Seq(mHandle, mdHandle))
  }


  private def scoped(parentScope: TScope, f: TScope => GeneratorContext): GeneratorContext = {
    val childScope: TScope = parentScope.enterScope()
    val generated = f(childScope)
    childScope.exitScope()
    generated
  }

  // TODO many types of pathelements...


}
