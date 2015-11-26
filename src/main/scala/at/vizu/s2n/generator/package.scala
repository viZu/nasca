package at.vizu.s2n

import at.vizu.s2n.generator.path.Expression

/**
  * Phil on 20.11.15.
  */
package object generator {

  type Path = Seq[Expression]

  implicit def stringToGeneratorContext(str: String): GeneratorContext = GeneratorContext(str)

  def generateExpressionChain(path: Path, seperator: String = "", endsWith: String = ""): GeneratorContext = {
    //val endsW = if(path.isEmpty || path.last.skipSemiColon) endsWith else endsWith + ";"
    GeneratorUtils.mergeGeneratorContexts(path.map(generateGeneratorCtx), seperator, endsWith)
  }

  private def generateGeneratorCtx(expr: Expression): GeneratorContext = {
    val ctx = expr.generate
    if (expr.skipSemiColon) ctx
    else ctx.enhance(ctx.content + ";")
  }

}
