package at.vizu.s2n

import at.vizu.s2n.generator.expression.Expression

/**
  * Phil on 20.11.15.
  */
package object generator {

  type Path = Seq[Expression]

  implicit def stringToGeneratorContext(str: String): GeneratorContext = GeneratorContext(str)

  def generateExpressionChain(path: Path, separator: String = "", endsWith: String = ""): GeneratorContext = {
    //val endsW = if(path.isEmpty || path.last.skipSemiColon) endsWith else endsWith + ";"
    GeneratorUtils.mergeGeneratorContexts(path.map(generateGeneratorCtx), separator, endsWith)
  }

  private def generateGeneratorCtx(expr: Expression): GeneratorContext = {
    val ctx = expr.generate
    if (expr.skipSemiColon || ctx.content.endsWith(";")) ctx
    else ctx.enhance(ctx.content + ";")
  }

}
