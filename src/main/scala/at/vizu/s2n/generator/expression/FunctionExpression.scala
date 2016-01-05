package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{Param, TScope, TType, TypeUtils}

/**
  * Phil on 02.01.16.
  */
case class FunctionExpression(scope: TScope, params: Seq[Param], body: BlockExpression, returnable: Boolean)
  extends Expression {
  override def exprTpe: TType = TypeUtils.createFunctionTypeFromParams(scope, params, body.exprTpe, 0)

  override def generate: GeneratorContext = {
    val bodyCtx: GeneratorContext = if (returnable) body.generateReturn else body.generate
    val paramsCtx = GeneratorUtils.generateParamsString(scope.baseTypes, params, withVars = true)
    val content = s"[&]($paramsCtx) $bodyCtx"
    GeneratorUtils.mergeGeneratorContexts(Vector(bodyCtx, paramsCtx), givenContent = content)
  }

  override def skipSemiColon: Boolean = false
}
