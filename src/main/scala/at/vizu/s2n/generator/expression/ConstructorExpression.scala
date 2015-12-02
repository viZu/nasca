package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.symbol.{BaseTypes, Method, TType}

/**
  * Phil on 29.11.15.
  */
case class ConstructorExpression(baseTypes: BaseTypes, method: Method, initMethodName: String) extends Expression {
  override def prevTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val typeName = GeneratorUtils.getCppTypeName(prevTpe.pkg, prevTpe.simpleName)
    val paramsString: String = GeneratorUtils.generateParamsString(baseTypes, method.params, withVars = true)
    val initMethodCall = s"this->$initMethodName()"
    val expressions: Seq[Expression] = (method.params.map(p => s"this->${p.name} = ${p.name}") :+ initMethodCall
      ).map(LiteralExpression(baseTypes.unit, _))
    val initializers = method.params.map(p => s"${p.name}(${p.name})").mkString(", ")
    val initializer = if (initializers.nonEmpty) s": $initializers " else ""
    val ctx: GeneratorContext = generateExpressionChain(expressions, "\n")
    val content: String =
      s"""$typeName::${prevTpe.simpleName}($paramsString) $initializer{
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content)
  }

  override def skipSemiColon: Boolean = true
}
