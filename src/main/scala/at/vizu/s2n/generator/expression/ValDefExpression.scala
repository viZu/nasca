package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 29.11.15.
  */
case class ValDefExpression(baseTypes: BaseTypes, varName: String, varTpe: TType, rhs: Expression) extends Expression {
  override def exprTpe: TType = null

  override def generate: GeneratorContext = {
    val tpe = if (varTpe != null) varTpe else rhs.exprTpe
    val typeName: GeneratorContext = GeneratorUtils.generateCppTypeName(baseTypes, tpe)
    val lhs = s"${typeName.content} $varName"
    val rhsCtx = rhs match {
      case b: BaseBlockExpression => rhs.returnContent
      case _ => rhs.content
    }
    val defString = s"$lhs = ${rhsCtx.content}"
    rhsCtx.enhance(defString, typeName.handles)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}
