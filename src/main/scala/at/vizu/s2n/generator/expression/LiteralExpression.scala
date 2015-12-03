package at.vizu.s2n.generator.expression

import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
case class LiteralExpression(tpe: TType, literal: String) extends Expression {

  def exprTpe = tpe

  def generate = {
    s"$literal"
  }

  override def skipSemiColon: Boolean = false
}
