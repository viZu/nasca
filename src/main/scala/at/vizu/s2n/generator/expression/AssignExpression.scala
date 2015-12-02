package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
case class AssignExpression(lhs: Expression, rhs: Expression) extends Expression {
  override def prevTpe: TType = null

  override def generate: GeneratorContext = {
    val lhsCtx = lhs.generate
    val rhsCtx = rhs match {
      case b: BaseBlockExpression => rhs.generateReturn
      case _ => rhs.generate
    }
    val generated = s"${lhsCtx.content} = ${rhsCtx.content}"
    GeneratorUtils.mergeGeneratorContexts(Seq(lhsCtx, rhsCtx), givenContent = generated)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}
