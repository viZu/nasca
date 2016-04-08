package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol._

/**
  * Phil on 29.11.15.
  */
case class InlineDefExpression(scope: TSymbolTable, method: Method, body: BaseBlockExpression) extends Expression {
  private val funcType = TypeUtils.createFunctionTypeFromParams(scope, method.params, method.returnType, 0)
  private val eagerBaseTypes = scope.baseTypes

  override def exprTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val bodyCtx: GeneratorContext = body.generate
    val paramsString = GeneratorUtils.generateParamsString(eagerBaseTypes, method.params, withVars = true)
    val funTpeString = GeneratorUtils.generateCppTypeName(eagerBaseTypes, funcType)
    val contentStr = s"$funTpeString ${method.name} = [&]($paramsString) ${bodyCtx.value}"
    bodyCtx.enhance(contentStr)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}
