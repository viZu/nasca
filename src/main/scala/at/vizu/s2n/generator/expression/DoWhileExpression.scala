package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 29.11.15.
  */
case class DoWhileExpression(baseTypes: BaseTypes, condExpr: Expression, body: Expression) extends Expression {
  override def prevTpe: TType = baseTypes.unit

  override def generate: GeneratorContext = {
    val condCtx = condExpr.generate
    val bodyCtx = body.generate
    val content = s"do ${bodyCtx.content} while (${condCtx.content})"
    GeneratorUtils.mergeGeneratorContexts(Seq(condCtx, bodyCtx), givenContent = content)
  }

  override def skipSemiColon: Boolean = true

  override def generateReturn: GeneratorContext = generate
}
