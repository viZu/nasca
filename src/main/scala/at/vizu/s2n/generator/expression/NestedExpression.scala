package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol._

/**
  * Phil on 29.11.15.
  */
case class NestedExpression(prevTpe: TType, varName: String, member: Member, params: Seq[Expression] = Seq()) extends Expression {
  def exprTpe = member.tpe

  def generate: GeneratorContext = {
    member match {
      case f: Field => generateFieldCallOnType(prevTpe, varName, f)
      case m: Method => generateMethodCallOnType(prevTpe, varName, m, params)
    }
  }

  private def generateMethodCallOnType(tpe: TType, varName: String, m: Method, params: Seq[Expression]): GeneratorContext = {
    val paramsContext = GeneratorUtils.mergeGeneratorContexts(params.map(_.generate), ", ")
    val paramsContent: String = paramsContext.content
    if (isOperator(m)) {
      val prettyOperator = prettifyOperator(m.name)
      paramsContext.enhance(s"$varName $prettyOperator $paramsContent")
    } else if (isNonPointerCall()) {
      paramsContext.enhance(s"$varName.${m.name}($paramsContent)")
    } else {
      val call = if (tpe.isObject) s"$varName->getInstance()->" else s"$varName->"
      paramsContext.enhance(s"$call${m.name}($paramsContent)")
    }
  }

  private def generateFieldCallOnType(tpe: TType, varName: String, f: Field): GeneratorContext = {
    val call = if (isNonPointerCall()) "." else "->"
    s"$varName$call${f.name}"
  }

  private def isOperator(m: Method): Boolean = m.name.contains("$")

  private def isNonPointerCall(): Boolean = false

  override def skipSemiColon: Boolean = true
}
