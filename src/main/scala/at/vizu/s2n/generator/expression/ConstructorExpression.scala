package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator._
import at.vizu.s2n.types.symbol.TScope

/**
  * Phil on 05.02.16.
  */
abstract class ConstructorExpression extends Expression {

  def scope: TScope

  def paramsString: GeneratorContext

  def initializer: String

  def bodyContent: Seq[Expression]

  override def generate: GeneratorContext = {
    val templateString = GeneratorUtils.generateClassTemplate(exprTpe)
    val ctx: GeneratorContext = generateExpressionChain(bodyContent, "\n")
    val content: String =
      s"""$templateString$typeName::${exprTpe.simpleName}($paramsString) $initializer{
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content, paramsString.handles)
  }

  def typeName = {
    GeneratorUtils.getCppTypeName(scope.baseTypes, exprTpe)
  }

  override def skipSemiColon: Boolean = true

}
