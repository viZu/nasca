package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{Constructor, TScope, TType}

/**
  * Phil on 05.02.16.
  */
case class SecondaryConstructorExpression(scope: TScope, constr: Constructor, primaryCallArgs: Seq[Expression],
                                          stats: Seq[Expression]) extends ConstructorExpression {

  override def exprTpe: TType = constr.returnType

  private def generatePrimaryCall = {
    val typeName: String = GeneratorUtils.getCppTypeName(scope.baseTypes, exprTpe).content
    val params: String = primaryCallArgs.map(_.content.content).mkString(", ")
    s"$typeName($params)"
  }

  override def skipSemiColon: Boolean = true

  override def paramsString: GeneratorContext = {
    GeneratorUtils.generateParamsString(scope.baseTypes, constr.params, withVars = true)
  }

  override def bodyContent: Seq[Expression] = stats

  override def initializer: String = s" : $generatePrimaryCall "

  override def constructor: Constructor = constr
}
