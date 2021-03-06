package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 29.11.15.
  */
abstract class BaseBlockExpression(stats: List[Expression], expr: Expression, returnable: Boolean = false) extends Expression {
  override def exprTpe: TType = expr.exprTpe

  override def generate: GeneratorContext = {
    generateAcc(returnable)
  }

  override def generateReturn: GeneratorContext = {
    generateAcc(true)
  }

  private def generateAcc(isReturnable: Boolean): GeneratorContext = {
    val statsStats = generateExpressionChain(stats, "\n")
    val bodyCtxList = Vector(statsStats, generateExpr(isReturnable))
    val bodyCtx: GeneratorContext = GeneratorUtils.mergeGeneratorContexts(bodyCtxList, "\n  ")
    generateContentStr(bodyCtx)
  }

  protected def generateContentStr(bodyCtx: GeneratorContext): GeneratorContext

  private def generateExpr(returnable: Boolean) = {
    val exprGenerate: GeneratorContext = if (returnable) expr.returnContent else expr.content
    val exprContent: String = exprGenerate.value
    exprGenerate.enhance(generateExprString(exprContent, expr.skipSemiColon))
  }

  private def generateExprString(exprContent: String, skipSemiColon: Boolean) = {
    if (skipSemiColon || exprContent.endsWith(";")) exprContent else exprContent + ";"
  }

}
