package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 29.11.15.
  */
case class ValDefExpression(baseTypes: BaseTypes, varName: String, rhs: Expression) extends Expression {
  override def exprTpe: TType = null

  override def generate: GeneratorContext = {
    val varTpe = rhs.exprTpe
    val lhs = s"${GeneratorUtils.generateCppTypeName(baseTypes, varTpe)} $varName"
    val rhsCtx = rhs match {
      case b: BaseBlockExpression => rhs.generateReturn
      case _ => rhs.generate
    }
    val defString = s"$lhs = ${rhsCtx.content}"
    rhsCtx.enhance(defString)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}
