package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.symbol.{BaseTypes, Method, TType}

/**
  * Phil on 29.11.15.
  */
case class ConstructorExpression(baseTypes: BaseTypes, method: Method, initMethodName: String) extends Expression {
  override def exprTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val typeName = GeneratorUtils.getCppTypeName(baseTypes, exprTpe) //GeneratorUtils.getCppTypeName(exprTpe.pkg, exprTpe.simpleName, "")
    val paramsString = GeneratorUtils.generateParamsString(baseTypes, method.params, withVars = true)
    val expressions: Seq[Expression] = generateInitMethodExpr()
    val initializers = method.params.map(p => s"${p.name}(${p.name})").mkString(", ")
    val initializer = if (initializers.nonEmpty) s": $initializers " else ""
    val ctx: GeneratorContext = generateExpressionChain(expressions, "\n")
    val templateString = GeneratorUtils.generateClassTemplate(exprTpe)
    val content: String =
      s"""$templateString$typeName::${exprTpe.simpleName}($paramsString) $initializer{
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content, paramsString.handles)
  }

  private def generateInitMethodExpr() = {
    if (initMethodName.isEmpty) Vector()
    else {
      val initMethodCall = s"this->$initMethodName()"
      Vector(LiteralExpression(baseTypes.unit, initMethodCall))
    }
  }

  override def skipSemiColon: Boolean = true
}
