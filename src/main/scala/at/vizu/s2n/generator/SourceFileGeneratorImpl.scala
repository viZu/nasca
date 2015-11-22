package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle, MethodDefinitionHandle, MethodHandle}
import at.vizu.s2n.generator.path.Expression
import at.vizu.s2n.types.TypeInference
import at.vizu.s2n.types.result.Implementation
import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

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
      //    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, method.returnType)
      TypeUtils.addParamsToScope(s, method.params)
      val paramsString = GeneratorUtils.generateParamsString(_baseTypes, method.params, withVars = true)
      //    val methodBody: GeneratorContext = generateMethodBody(scope, d.rhs, _baseTypes.unit != method.returnType)
      //    val methodString: String = s"""$returnTypeString ${method.name}($paramsString) ${methodBody.content}"""
      //    methodBody.enhance(methodString)
      generateMethod(s, d.rhs, method.returnType, method.name, paramsString)
    })
  }

  private def generateMethodBody(scope: TScope, body: Tree, returnable: Boolean): GeneratorContext = {
    body match {
      case b: Block => generateStatementBlock(scope, b, returnable)
      case _ => forceGenerateBlock(scope, body, returnable)
    }
  }

  private def generateField(scope: TScope, v: ValDef): GeneratorContext = {
    val field: Field = TypeUtils.findField(scope, v, tpe)
    generateFieldBody(scope, v.rhs, field)
  }

  private def generateFieldBody(scope: TScope, body: Tree, field: Field): GeneratorContext = {
    body match {
      case b: Block => generateInitMethod(scope, b, field.name, field.tpe).removeContent()
      case _@s =>
        val expression: GeneratorContext = generateExpression(scope, s, returnable = false)
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

  private def generateMethod(scope: TScope, rhs: Tree, returnType: TType, methodName: String, paramsString: String = ""): GeneratorContext = {
    val cppMethodName = getMethodName(methodName)
    val methodBody: GeneratorContext = generateMethodBody(scope, rhs, _baseTypes.unit != returnType)
    val returnTypeString = GeneratorUtils.generateCppTypeName(_baseTypes, returnType)
    val methodString: String = s"""$returnTypeString $cppMethodName($paramsString) ${methodBody.content}"""
    methodBody.enhance(methodString)
  }

  private def getMethodName(methodName: String) = GeneratorUtils.getCppTypeName(_baseTypes, tpe) + "::" + methodName

  private def generateBlock(scope: TScope, block: Block, returnable: Boolean,
                            textGenerator: (GeneratorContext, GeneratorContext) => String): GeneratorContext = {
    scoped(scope, (s: TScope) => {
      val statements = generateStatements(s, block.stats)
      val expression = generateExpression(s, block.expr, returnable)
      val content = textGenerator(statements, expression)
      GeneratorContext(content, statements.handles ++ expression.handles)
    })
  }


  private def generateExpressionBlock(scope: TScope, block: Block, returnable: Boolean,
                                      lambdaParen: String = "()", endsWith: String = "()"): GeneratorContext = {
    val startsWith = s"[this,&]$lambdaParen"
    val textGenerator = (statements: GeneratorContext, expression: GeneratorContext) => {
      //TODO expression Block with lambda...
      s"""$startsWith{
         | ${statements.content}
         | ${expression.content}
         |}$endsWith""".stripMargin
    }
    generateBlock(scope, block, returnable = true, textGenerator)
  }

  private def generateStatementBlock(scope: TScope, block: Block, returnable: Boolean): GeneratorContext = {
    val textGenerator = (statements: GeneratorContext, expression: GeneratorContext) => {
      s"""{
          | ${statements.content}
          | ${expression.content}
          |}""".stripMargin
    }
    generateBlock(scope, block, returnable, textGenerator)
  }

  private def forceGenerateBlock(scope: TScope, tree: Tree, returnable: Boolean = false): GeneratorContext = {
    tree match {
      case b: Block => generateStatementBlock(scope, b, returnable)
      case _ => generateStatementBlock(scope, Block(Nil, tree), returnable)
    }
  }

  private def forceGenerateListBlock(scope: TScope, trees: List[Tree], returnable: Boolean = false): GeneratorContext = {
    val block: Block = trees match {
      case expr :: Nil => Block(Nil, expr)
      case _ => Block(trees.dropRight(1), trees.last)
    }
    generateStatementBlock(scope, block, returnable)
  }

  private def generateStatements(scope: TScope, statements: List[Tree]): GeneratorContext = {
    GeneratorUtils.mergeGeneratorContexts(statements.map(generateStatement(scope, _)))
  }

  private def generateStatement(scope: TScope, statement: Tree): GeneratorContext = {
    statement match {
      case b: Block => generateStatementBlock(scope, b, returnable = false)
      case v: ValDef => generateValDef(scope, v)
      case d: DefDef => generateNestedMethodDef(scope, d)
      case a: Assign => generateAssign(scope, a)
      case a: Apply => generateApply(scope, a)
      case s: Select => generateSelect(scope, s)
      case i: Ident => generateIdent(scope, i)
      case l: Literal => generateLiteral(scope, l)
      case i: If => generateIf(scope, i)
      case l: LabelDef => generateLabelDef(scope, l)
      case f: Function => throw new TypeException(scope.currentFile, 0, "Anonymous functions are currently not supported")
      case EmptyTree => println("EmptyTree!"); ""
    }
  }

  private def generateExpression(scope: TScope, expression: Tree, returnable: Boolean): GeneratorContext = {
    val expr: GeneratorContext = generateStatement(scope, expression)
    val content: String = if (returnable) "return " + expr.content else expr.content
    expr.enhance(content)
  }

  private def generateValDef(scope: TScope, v: ValDef): GeneratorContext = {
    val varTpe = findVarType(scope, v.tpt, v.rhs)
    val varName = v.name.toString
    TypeUtils.createIdentifier(scope, v, varTpe)
    val rhs = generateValRhs(scope, v.rhs)
    val lhs = s"${GeneratorUtils.generateCppTypeName(_baseTypes, varTpe)} $varName"
    val valStr = s"$lhs = ${rhs.content};"

    rhs.enhance(valStr)
  }

  private def findVarType(scope: TScope, tpt: Tree, rhs: Tree) = {
    val varTpe = TypeUtils.findType(scope, tpt)
    if (varTpe == null) TypeInference.getType(_baseTypes, scope, rhs) else varTpe
  }

  private def generateValRhs(scope: TScope, body: Tree): GeneratorContext = {
    body match {
      case b: Block =>
        generateExpressionBlock(scope, b, returnable = true)
      //generateInitMethod(scope, b, name, tpe)
      case _@s =>
        // TODO remove toString
        generateStatement(scope, s)
    }
  }

  private def generateNestedMethodDef(scope: TScope, d: DefDef): GeneratorContext = {
    Expression(_baseTypes, scope, d).generate
    //      val nestedMethod: Method = TypeUtils.createMethod(s, d, instanceMethod = false)
    //      TypeUtils.addParamsToScope(s, nestedMethod.params)
    //      scope.addMethod(nestedMethod)
    //      val paramsString = GeneratorUtils.generateParamsString(_baseTypes, nestedMethod.params, withVars = true)
    //      val bodyCtx: GeneratorContext = generateMethodBody(s, d.rhs, nestedMethod.returnType != _baseTypes.unit)
    //      val methodDef = s"auto ${nestedMethod.name} = [&,this]($paramsString) ${bodyCtx.content};"
    //      bodyCtx.enhance(methodDef)
  }

  private def generateAssign(scope: TScope, a: Assign): GeneratorContext = {
    val (lhsString, tpe) = a.lhs match {
      case _@tree =>
        val expr = Expression(_baseTypes, scope, tree)
        (expr.generate.content, expr.prevTpe)
    }

    val body: GeneratorContext = generateValRhs(scope, a.rhs)
    body.enhance(s"$lhsString = ${body.content};")
  }

  private def generateIdentAssignLhs(scope: TScope, i: Ident): (String, TType) = {
    val identifier = TypeUtils.findIdentifier(scope, i)

    val lhs = if (identifier.fromField) s"this->${identifier.name}" else identifier.name

    (lhs, identifier.tpe)
  }

  private def generateApply(scope: TScope, a: Apply): GeneratorContext = {
    val generate: GeneratorContext = Expression(_baseTypes, scope, a).generate
    generate.enhance(generate.content + ";")
    //generateExpressionChain(Expression(_baseTypes, scope, a), endsWith = ";")
  }

  private def generateSelect(scope: TScope, s: Select): GeneratorContext = {
    val generate: GeneratorContext = Expression(_baseTypes, scope, s).generate
    generate.enhance(generate.content + ";")
    //    generateExpressionChain(, endsWith = ";")
  }


  private def generateIdent(scope: TScope, ident: Ident): GeneratorContext = {
    val iName: String = ident.name.toString
    TypeUtils.findIdent(scope, iName) match {
      case m: Method =>
        if (m.instanceMethod) s"this->${m.name}();"
        else GeneratorUtils.generateScopeMethod(iName) + ";"
      case i: Identifier => ident.name + ";"
      case f: Field => s"this->${f.name};"
      case _ => throw new RuntimeException("Todo")
    }
  }

  private def generateLiteral(scope: TScope, l: Literal): GeneratorContext = {
    l.value.value.toString
  }

  private def generateIf(scope: TScope, i: If): GeneratorContext = {
    val condition = i.cond.toString() // TODO generate condition
    val thenBody = i.thenp
    s"if(condition) "
  }

  private def generateThenPart(scope: TScope, thenP: Tree): GeneratorContext = {
    forceGenerateBlock(scope, thenP)
  }

  private def generateElsePart(scope: TScope, elseP: Tree): GeneratorContext = {
    elseP match {
      case l: Literal => l.value.value match {
        case bu: BoxedUnit => ""
        case _ => forceGenerateBlock(scope, l)
      }
      case _ => forceGenerateBlock(scope, elseP)
    }
  }

  private def generateLabelDef(scope: TScope, l: LabelDef): GeneratorContext = {
    l match {
      case LabelDef(n, _, If(cond, body, _)) => generateWhile(scope, cond, body)
      case LabelDef(n, _, Block(body, If(cond, _, _))) => generateDoWhile(scope, cond, body)
    }
  }

  private def generateWhile(scope: TScope, cond: Tree, body: Tree): GeneratorContext = {
    val bodyContext: GeneratorContext = forceGenerateBlock(scope, body)
    val condition = cond.toString() // TODO generate condition
    bodyContext.enhance(s"while($condition) $bodyContext")
  }

  private def generateDoWhile(scope: TScope, cond: Tree, bodyList: List[Tree]): GeneratorContext = {
    val bodyContext = forceGenerateListBlock(scope, bodyList)
    val condition = cond.toString() // TODO generate condition
    bodyContext.enhance(s"do ${bodyContext.content} while ($condition)")
  }

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
