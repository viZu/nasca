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
  * Phil on 12.02.16.
  */
class CppTemplateFileGenerator(baseTypes: BaseTypes, classScope: TScope, implementation: Implementation)
  extends TemplateFileGenerator with LazyLogging {

  def tpe = implementation.tpe

  val packageName = tpe.pkg
  lazy val classInitMethodName = "__init__class__" + implementation.tpe.simpleName

  override def generateTemplateFile(args: Arguments): Unit = {
    val name = GeneratorUtils.getHeaderFileName(tpe)
    logger.debug("Generating template file {}", name)
    addGenericsToScope(classScope)
    val includeGuard = GeneratorUtils.generateIncludeGuard(packageName, GeneratorUtils.getHeaderFileName(tpe))

    val templateContent: GeneratorContext = generateTemplateContent()
    val includes = generateIncludes(templateContent.handles)
    val content: String = includeGuard + includes + templateContent.content

    logger.debug("Writing template file {}", name)
    val prettyContent = CodePrettifier.prettify(content)
    ScalaFiles.writeToFile(args.generatedDir, name, prettyContent)
  }

  private def addGenericsToScope(scope: TScope) = {
    implementation.tpe match {
      case g: GenericType => g.genericModifiers.foreach(scope.addClass)
      case _ =>
    }
  }

  private def generateIncludes(handles: Set[GeneratorHandle]): String = {
    val usedTypes = TypeUtils.getUsedTypes(baseTypes, tpe)
    (GeneratorUtils.generateIncludes(usedTypes) ++ generateIncludesAcc(handles)).toSet.mkString("\n") + "\n\n"
  }

  private def generateIncludesAcc(handles: Set[GeneratorHandle]) = {
    val usedHeader: Set[String] = TypeUtils.getUsedTypes(baseTypes, tpe).map(GeneratorUtils.getHeaderFileName)
    val tpeHeaderFile = GeneratorUtils.getHeaderFileName(tpe)
    val includeHandles = handles.collect({ case ih: IncludeHandle => ih })
      .filter(handle => !usedHeader.contains(handle.cppInclude) && handle.cppInclude != tpeHeaderFile)
    includeHandles.toSeq.map(_.content)
  }

  private def generateTemplateContent() = {
    val sections = generateSections(classScope)
    val classBody = GeneratorUtils.generateClassBody(baseTypes, tpe, sections.content)
    val content = GeneratorUtils.wrapBodyWithNamespace(packageName, classBody) + GeneratorUtils.generateClassEnding(tpe)
    sections.enhance(content)
  }

  protected def generateSections(scope: TScope): GeneratorContext = {
    GeneratorUtils.mergeGeneratorContexts(groupMember(scope).map(p => generateSection(p._1, p._2)).toSeq, "\n\n")
  }

  protected def generateSection(visibility: String, members: Seq[GeneratorContext]): GeneratorContext = {
    visibility match {
      case "public" =>
        val copyConstructors: GeneratorContext = GeneratorUtils.generateCopyConstructors(baseTypes, tpe)
        generateVisibilitySection(visibility, members :+ copyConstructors)
      case _ => generateVisibilitySection(visibility, members)
    }
  }

  protected def generateVisibilitySection(visibility: String, members: Seq[GeneratorContext]): GeneratorContext = {
    val contexts: GeneratorContext = GeneratorUtils.mergeGeneratorContexts(members, "\n\n")
    val memberStr: GeneratorContext = if (members.isEmpty) "" else contexts.enhance("\n" + contexts.content)
    contexts.enhance(s"""$visibility:$memberStr""".stripMargin)
  }

  private def groupMember(scope: TScope): Map[String, Seq[GeneratorContext]] = {
    val (memberTrees, constructorContent) = implementation.tree.impl.body.partition({
      case v: ValOrDefDef => true
      case _ => false
    })

    val initCtx = ("private", generateContructorInit(scope, constructorContent))
    val members = memberTrees.flatMap({
      case d: DefDef => Seq(generateMethod(scope, d, initCtx._2))
      case v: ValDef => generateField(scope, v)
    }) :+ initCtx
    members.groupBy(_._1).mapValues(sq => sq.map(_._2))
  }

  private def generateMethod(scope: TScope, d: DefDef, initCtx: GeneratorContext): (String, GeneratorContext) = {
    scope.scoped((s: TScope) => {
      TypeUtils.createAndAddGenericModifiers(s, d.tparams)
      val method: Method = TypeUtils.findMethodForDef(s, d)
      val ctx: GeneratorContext = if (method.isAbstract || tpe.isTrait && method.constructor) {
        GeneratorUtils.generateVirtualMethod(baseTypes, method)
      } else if (method.constructor) generateConstructor(s, method, initCtx, d)
      else {
        TypeUtils.addParamsToScope(s, method.params)
        generateMethod(s, d.rhs, method)
      }
      (method.visibility, ctx)
    }, MethodScope)
  }

  private def generateConstructor(scope: TScope, method: Method, initCtx: GeneratorContext,
                                  d: DefDef): GeneratorContext = method match {
    case c: Constructor if c.primary =>
      if (initCtx.isEmpty) PrimaryConstructorExpression(scope, c, "").generateForHeader // no in
      else PrimaryConstructorExpression(scope, c, classInitMethodName).generateForHeader
    case c: Constructor =>
      scope.scoped((childScope: TScope) => {
        TypeUtils.addParamsToScope(scope, c.params)
        val block = Expression.wrapInBlock(d.rhs)
        val stats: List[Expression] = block.stats.tail.map(Expression(scope.baseTypes, scope, _))
        val primaryCallArgs = block.stats.head match {
          case Apply(i: Ident, args) if i.name.toString == TypeUtils.ConstructorName => Expression(scope, args)
        }
        SecondaryConstructorExpression(scope, c, primaryCallArgs, stats).generateForHeader
      }, MethodScope)
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
    val mdHandle = generateInitMethodHandle(classInitMethodName, baseTypes.unit) // TODO do I need this?
    val method = mdHandle.method
    val methodCtx = generateMethod(scope, body, method)
    val mHandle = MethodHandle(methodCtx.content)

    methodCtx.enhance("", Set(mHandle, mdHandle))
  }

  private def generateInitMethodHandle(methodName: String, fieldTpe: TType) = {
    val m: Method = Method(Context("", 0), methodName, fieldTpe, Vector(Private))
    MethodDefinitionHandle(m)
  }

  private def generateMethod(scope: TScope, rhs: Tree, method: Method): GeneratorContext = {
    val rhsBlock: Block = Expression.wrapInBlock(rhs)
    val methodBody: GeneratorContext = Expression
      .getBaseBlockExpression(baseTypes, scope, rhsBlock, method.tpe != baseTypes.unit).generate
    val methodDef = GeneratorUtils.generateMethodDefinition(baseTypes, method, withSemicolon = false)
    val methodString: String = s"""$methodDef ${methodBody.content}"""
    GeneratorUtils.mergeGeneratorContexts(Seq(methodBody, methodDef), givenContent = methodString)
  }

  private def generateField(scope: TScope, v: ValDef): Seq[(String, GeneratorContext)] = {
    val field: Field = TypeUtils.findField(scope, v, tpe)
    generateFieldBody(scope, v.rhs, field)
  }

  private def generateFieldBody(scope: TScope, body: Tree, field: Field): Seq[(String, GeneratorContext)] = {
    val definition = GeneratorUtils.generateFieldDefinition(baseTypes, field)
    body match {
      case b: Block =>
        val initMethodName = "__init__" + field.name.toUpperCase
        val initCall = initMethodName + "()"
        val initCtx = generateInitMethod(scope, b, initMethodName, initCall, field.name, field.tpe).removeContent()
        val initMethod = ("private", initCtx)
        val fieldDefinition = (field.visibility, GeneratorContext(definition + initCall + ";"))
        Seq(fieldDefinition, initMethod)
      case EmptyTree => Seq((field.visibility, GeneratorContext(definition + ";")))
      case _ =>
        val expr: Expression = Expression(baseTypes, scope, body)
        val expression: GeneratorContext = generateExpression(scope, expr, returnable = false)
        val handle = FieldInitializerHandle(field.name, expression.content)
        val initializer = GeneratorUtils.generateFieldInitializer(handle)
        Seq((field.visibility, expression.enhance(definition + initializer)))
    }
  }

  private def generateInitMethod(scope: TScope, b: Block, initMethodName: String, initCall: String,
                                 varName: String, varTpe: TType): GeneratorContext = {
    val privateMethodHandle = generateInitMethodHandle(initMethodName, varTpe)
    val method = privateMethodHandle.method
    generateMethod(scope, b, method)
  }

  private def generateExpression(scope: TScope, expression: Expression, returnable: Boolean): GeneratorContext = {
    val exprCtx: GeneratorContext = expression.generate
    val content: String = if (returnable) "return " + exprCtx.content else exprCtx.content
    exprCtx.enhance(content)
  }

}
