package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 07.04.16.
  */
case class ThisExpression(baseTypes: BaseTypes, tpe: TType) extends Expression {
  override def exprTpe: TType = tpe

  override protected def generate: GeneratorContext = {
    val name: GeneratorContext = GeneratorUtils.generateCppTypeName(baseTypes, tpe)
    s"$name(this)"
  }

  override def skipSemiColon: Boolean = false
}
