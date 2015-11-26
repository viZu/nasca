package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle, MethodDefinitionHandle, MethodHandle}
import at.vizu.s2n.generator.path.Expression
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._

/**
  * Phil on 12.11.15.
  */
class SourceFileGeneratorImpl(_baseTypes: BaseTypes, classScope: TScope, implementation: Implementation) extends SourceFileGenerator {

  def tpe = implementation.tpe

  def nameSpace = GeneratorUtils.getNameSpace(tpe.pkg)

  override def generateSourceFile(args: Arguments): Seq[GeneratorHandle] = {
    val name = GeneratorUtils.getSourceFileName(tpe)
    println("Generating source file " + name)

    val context = generateContent(classScope)

    println("Writing source file " + name)
    ScalaFiles.writeToFile(args.out, name, context.content)
    context.handles
  }

  private def generateContent(scope: TScope): GeneratorContext = {
    val context = generateContentAcc(scope)
    context.enhance( s"""#include "${GeneratorUtils.getHeaderFileName(tpe)}"""" + "\n\n" + context.content)
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
    val members: List[GeneratorContext] = implementation.tree.impl.body.map({
      case d: DefDef => generateMethod(scope, d)
      case v: ValDef => generateField(scope, v)
      case _ => null
    }).filter(_ != null)
    GeneratorUtils.mergeGeneratorContexts(members, "\n\n") // remove unhandled member and contexts
  }

  private def generateMethod(scope: TScope, d: DefDef): GeneratorContext = {
    val method: Method = TypeUtils.findMethodForDef(scope, d)
    if (method.constructor) return GeneratorContext()
    scoped(scope, (s: TScope) => {
      TypeUtils.addParamsToScope(s, method.params)
      generateMethod(s, d.rhs, method.returnType, method.name, method.params)
    })
  }

  private def generateField(scope: TScope, v: ValDef): GeneratorContext = {
    val field: Field = TypeUtils.findField(scope, v, tpe)
    generateFieldBody(scope, v.rhs, field)
  }

  private def generateFieldBody(scope: TScope, body: Tree, field: Field): GeneratorContext = {
    body match {
      case b: Block => generateInitMethod(scope, b, field.name, field.tpe).removeContent()
      case _@s =>
        val expr: Expression = Expression(_baseTypes, scope, s)
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

  private def generateIf(scope: TScope, i: If): GeneratorContext = {
    val condition = i.cond.toString() // TODO generate condition
    val thenBody = i.thenp
    s"if(condition) "
  }

  //  private def generateThenPart(scope: TScope, thenP: Tree): GeneratorContext = {
  //    forceGenerateBlock(scope, thenP)
  //  }
  //
  //  private def generateElsePart(scope: TScope, elseP: Tree): GeneratorContext = {
  //    elseP match {
  //      case l: Literal => l.value.value match {
  //        case bu: BoxedUnit => ""
  //        case _ => forceGenerateBlock(scope, l)
  //      }
  //      case _ => forceGenerateBlock(scope, elseP)
  //    }
  //  }

  //  private def generateLabelDef(scope: TScope, l: LabelDef): GeneratorContext = {
  //    Expression(_baseTypes, scope, l).generate
  //    l match {
  //      case LabelDef(n, _, If(cond, body, _)) => generateWhile(scope, cond, body)
  //      case LabelDef(n, _, Block(body, If(cond, _, _))) => generateDoWhile(scope, cond, body)
  //    }
  //  }

  //  private def generateWhile(scope: TScope, cond: Tree, body: Tree): GeneratorContext = {
  //    val bodyContext: GeneratorContext = forceGenerateBlock(scope, body)
  //    val condition = cond.toString() // TODO generate condition
  //    bodyContext.enhance(s"while($condition) $bodyContext")
  //  }
  //
  //  private def generateDoWhile(scope: TScope, cond: Tree, bodyList: List[Tree]): GeneratorContext = {
  //    val bodyContext = forceGenerateListBlock(scope, bodyList)
  //    val condition = cond.toString() // TODO generate condition
  //    bodyContext.enhance(s"do ${bodyContext.content} while ($condition)")
  //  }

  private def generateConstructorContent(): String = {
    ""
  }

  private def scoped(parentScope: TScope, f: TScope => GeneratorContext): GeneratorContext = {
    val childScope: TScope = parentScope.enterScope()
    val generated = f(childScope)
    childScope.exitScope()
    generated
  }

  // TODO many types of pathelements...


}
