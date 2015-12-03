package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
case class ChainedExpression(path: Path) extends Expression {
  override def exprTpe: TType = path.last.exprTpe

  override def generate: GeneratorContext = generateExpressionChain(path)

  override def skipSemiColon: Boolean = false
}
