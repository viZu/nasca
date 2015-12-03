package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.GeneratorContext
import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
case class EmptyExpression(unit: TType) extends Expression {
  override def exprTpe: TType = unit

  override def generate: GeneratorContext = GeneratorContext()

  override def skipSemiColon: Boolean = true

  override def generateReturn: GeneratorContext = generate
}
