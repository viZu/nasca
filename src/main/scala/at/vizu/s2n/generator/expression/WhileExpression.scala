package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 29.11.15.
  */
case class WhileExpression(baseTypes: BaseTypes, condExpr: Expression, body: Expression) extends Expression {
  override def exprTpe: TType = baseTypes.unit

  override def generate: GeneratorContext = {
    val condCtx = condExpr.content
    val bodyCtx = body.content
    val content = s"while(${condCtx.value}) ${bodyCtx.value}"
    GeneratorUtils.mergeGeneratorContexts(Vector(condCtx, bodyCtx), givenContent = content)
  }

  override def skipSemiColon: Boolean = true

  override def generateReturn: GeneratorContext = generate
}
