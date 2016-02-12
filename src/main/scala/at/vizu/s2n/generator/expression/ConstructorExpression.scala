package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator._
import at.vizu.s2n.types.symbol.{Constructor, TScope}

/**
  * Phil on 05.02.16.
  */
abstract class ConstructorExpression extends Expression {

  def scope: TScope

  def constructor: Constructor

  def paramsString: GeneratorContext

  def initializer: String

  def bodyContent: Seq[Expression]

  override def generate: GeneratorContext = {
    val templateString = GeneratorUtils.generateClassTemplate(exprTpe)
    val ctx: GeneratorContext = generateExpressionChain(bodyContent, "\n")
    val simpleName = GeneratorUtils.getSimpleName(scope.baseTypes, constructor.tpe)
    val content: String =
      s"""$templateString$typeName::$simpleName($paramsString) $initializer{
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content) ++ paramsString.handles
  }

  def generateForHeader: GeneratorContext = {
    val definition = GeneratorUtils.generateConstructorDefinition(scope.baseTypes, constructor, withSemicolon = false)
    val ctx: GeneratorContext = generateExpressionChain(bodyContent, "\n")
    val content =
      s"""$definition $initializer{
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content) ++ definition.handles
  }

  def typeName = {
    GeneratorUtils.getCppTypeName(scope.baseTypes, exprTpe)
  }

  override def skipSemiColon: Boolean = true

}
