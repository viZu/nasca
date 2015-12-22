package at.vizu.s2n

import at.vizu.s2n.generator.expression.Expression
import at.vizu.s2n.generator.handles.GeneratorHandle

/**
  * Phil on 20.11.15.
  */
package object generator {

  type Path = Seq[Expression]

  implicit def stringToGeneratorContext(str: String): GeneratorContext = GeneratorContext(str)

  implicit def handleToGeneratorContext(optHandle: Option[GeneratorHandle]): GeneratorContext = optHandle match {
    case None => GeneratorContext()
    case Some(handle) => GeneratorContext(handles = Set(handle))
  }

  def generateExpressionChain(path: Path, separator: String = "", endsWith: String = "",
                              skipSemiColon: Boolean = false): GeneratorContext = {
    //val endsW = if(path.isEmpty || path.last.skipSemiColon) endsWith else endsWith + ";"
    GeneratorUtils.mergeGeneratorContexts(path.map(generateGeneratorCtx(_, skipSemiColon)), separator, endsWith)
  }

  private def generateGeneratorCtx(expr: Expression, skipSemiColon: Boolean = true): GeneratorContext = {
    val ctx = expr.generate
    if (skipSemiColon || expr.skipSemiColon || ctx.content.endsWith(";")) ctx
    else ctx + ";"
  }

}
