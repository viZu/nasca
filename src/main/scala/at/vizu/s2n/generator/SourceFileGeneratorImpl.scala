package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle, MethodDefinitionHandle, MethodHandle}
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._

/**
  * Phil on 12.11.15.
  */
class SourceFileGeneratorImpl(_baseTypes: BaseTypes, classScope: TScope, implementation: Implementation) extends SourceFileGenerator {

  def tpe = implementation.tpe

  def nameSpace = GeneratorUtils.getNameSpace(tpe.pkg)

  implicit def stringToGeneratorContext(str: String): GeneratorContext = GeneratorContext(str)

  def mergeGeneratorContexts(seq: Seq[GeneratorContext], seperator: String = "\n"): GeneratorContext = {
    val content: String = seq.map(_.content).mkString(seperator)
    val handles: Seq[GeneratorHandle] = seq.flatMap(_.handles)
    GeneratorContext(content, handles)
  }

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
    mergeGeneratorContexts(implementation.tree.impl.body.map({
      case d: DefDef => generateMethod(scope, d)
      case v: ValDef => generateField(scope, v)
      case _ => null
    }).filter(_ != null), "\n\n")
  }

  private def generateMethod(scope: TScope, d: DefDef): GeneratorContext = {
    val method: Method = TypeUtils.findMethodForDef(scope, d)
    //    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, method.returnType)
    val paramsString = GeneratorUtils.generateParamsString(_baseTypes, method.params, withVars = true)
    //    val methodBody: GeneratorContext = generateMethodBody(scope, d.rhs, _baseTypes.unit != method.returnType)
    //    val methodString: String = s"""$returnTypeString ${method.name}($paramsString) ${methodBody.content}"""
    //    methodBody.enhance(methodString)
    generateMethod(scope, d.rhs, method.returnType, method.name, paramsString)
  }

  private def generateMethodBody(scope: TScope, body: Tree, returnable: Boolean): GeneratorContext = {
    body match {
      case b: Block => generateBlock(scope, b, returnable)
      case _ =>
        val ret = if (returnable) "return " else ""
        s"""{
            |  $ret${body.toString()};
            |}""".stripMargin //TODO temporary
    }
  }

  private def generateField(scope: TScope, v: ValDef): GeneratorContext = {
    val field: Field = TypeUtils.findField(scope, v, tpe)
    generateFieldBody(scope, v.rhs, field)
  }

  private def generateFieldBody(scope: TScope, body: Tree, field: Field): GeneratorContext = {
    body match {
      case b: Block => generateInitMethod(scope, b, field)
      case _@s =>
        // TODO field initializer handle remove toString
        GeneratorContext(handles = Seq(FieldInitializerHandle(field.name, s.toString())))
    }
  }

  private def generateInitMethod(scope: TScope, b: Block, field: Field): GeneratorContext = {
    val initMethodName = "init$$" + field.name.toUpperCase
    val initCall = initMethodName + "()"
    val initializerHandle = FieldInitializerHandle(field.name, initCall)
    val privateMethodHandle = generateInitMethodHandle(initMethodName, field.tpe)
    generateMethod(scope, b, field.tpe, initMethodName).enhance(Seq(initializerHandle, privateMethodHandle))
  }

  private def generateInitMethodHandle(methodName: String, fieldTpe: TType) = {
    val m: Method = Method(Context("", 0), methodName, fieldTpe, Seq(Private))
    MethodDefinitionHandle(m)
  }

  private def generateMethod(scope: TScope, rhs: Tree, returnType: TType, methodName: String, paramsString: String = ""): GeneratorContext = {
    val cppMethodName = getMethodName(methodName)
    val methodBody: GeneratorContext = generateMethodBody(scope, rhs, _baseTypes.unit != returnType)
    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, returnType)
    val methodString: String = s"""$returnTypeString $cppMethodName($paramsString) ${methodBody.content}"""
    methodBody.enhance(methodString)
  }

  private def getMethodName(methodName: String) = GeneratorUtils.getCppTypeName(_baseTypes, tpe) + "::" + methodName

  private def generateBlock(scope: TScope, block: Block, returnable: Boolean): GeneratorContext = {
    scoped(scope, (s: TScope) => {
      val statements = generateStatements(s, block.stats)
      val expression = generateExpression(s, block.expr, returnable)
      val content =
        s"""{
            | ${statements.content}
            | ${expression.content}
            |}""".stripMargin
      GeneratorContext(content, statements.handles ++ expression.handles)
    })
  }

  private def generateStatements(scope: TScope, statements: List[Tree]): GeneratorContext = {
    mergeGeneratorContexts(statements.map(generateStatement(scope, _)))
  }

  private def generateStatement(scope: TScope, statement: Tree): GeneratorContext = {
    statement.toString() + ";"
  }

  private def generateExpression(scope: TScope, expression: Tree, returnable: Boolean): GeneratorContext = {
    (if (returnable) "return " + expression.toString() else expression.toString()) + ";"
  }

  private def generateConstructorContent(): String = {
    ""
  }

  private def scoped(parentScope: TScope, f: TScope => GeneratorContext): GeneratorContext = {
    if (parentScope.isEmptyScope()) {
      f(parentScope)
    } else {
      val childScope: TScope = parentScope.enterScope()
      val generated = f(childScope)
      childScope.exitScope()
      generated
    }
  }
}
