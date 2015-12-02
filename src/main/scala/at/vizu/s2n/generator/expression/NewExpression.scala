package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.GeneratorUtils
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 29.11.15.
  */
case class NewExpression(baseTypes: BaseTypes, tpe: TType, params: Seq[Expression] = Seq()) extends Expression {
  def prevTpe = tpe

  def generate = {
    val cppName = GeneratorUtils.getCppTypeName(baseTypes, tpe)
    val paramsContext = GeneratorUtils.mergeGeneratorContexts(params.map(_.generate), ", ")
    val sharedPtrName = GeneratorUtils.generateCppTypeName(baseTypes, tpe)
    paramsContext.enhance(s"$sharedPtrName(new $cppName(${paramsContext.content}))")
  }

  override def skipSemiColon: Boolean = false
}
