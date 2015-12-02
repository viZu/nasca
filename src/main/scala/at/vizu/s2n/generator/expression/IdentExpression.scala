package at.vizu.s2n.generator.expression

import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
case class IdentExpression(tpe: TType, expr: String) extends Expression {
  def prevTpe = tpe

  def generate = {
    expr
  }

  override def skipSemiColon: Boolean = false
}
