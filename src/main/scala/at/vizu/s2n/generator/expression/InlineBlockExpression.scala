package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.GeneratorContext

/**
  * Phil on 29.11.15.
  */
case class InlineBlockExpression(stats: List[Expression], expr: Expression, returnable: Boolean = false)
  extends BaseBlockExpression(stats, expr, returnable) {
  protected def generateContentStr(bodyCtx: GeneratorContext) = {
    val contentStr =
      s"""[&]() {
          |  ${bodyCtx.content}
          |}()""".stripMargin
    bodyCtx.enhance(contentStr)
  }

  override def skipSemiColon: Boolean = false
}
