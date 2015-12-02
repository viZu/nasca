package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{BaseTypes, Method, TType}

/**
  * Phil on 29.11.15.
  */
case class InlineDefExpression(baseTypes: BaseTypes, method: Method, body: BaseBlockExpression) extends Expression {
  override def prevTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val bodyCtx: GeneratorContext = body.generate
    val paramsString = GeneratorUtils.generateParamsString(baseTypes, method.params, withVars = true)
    val contentStr = s"auto ${method.name} = [&]($paramsString) ${bodyCtx.content}"
    bodyCtx.enhance(contentStr)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}
