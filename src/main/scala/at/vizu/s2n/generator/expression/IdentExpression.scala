package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.GeneratorContext
import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
case class IdentExpression(tpe: TType, expr: GeneratorContext) extends Expression {
  def exprTpe = tpe

  def generate = expr

  override def skipSemiColon: Boolean = false
}
