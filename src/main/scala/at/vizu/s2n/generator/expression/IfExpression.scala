package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.symbol.{BaseTypes, TScope, TType, TypeUtils}

/**
  * Phil on 29.11.15.
  */
case class IfExpression(baseTypes: BaseTypes, scope: TScope, ifParts: Seq[IfPart], elseP: Expression) extends Expression {
  override def prevTpe: TType = {
    TypeUtils.findCommonBaseClass(scope, ifParts.head.body.prevTpe, elseP.prevTpe)
  }

  override def generate: GeneratorContext = generateAcc(false)

  override def generateReturn: GeneratorContext = generateAcc(true)

  private def generateAcc(withReturn: Boolean) = {
    val parts = Seq(generateIfPart(withReturn), generateElseIfParts(withReturn), generateElsePart(withReturn))
    GeneratorUtils.mergeGeneratorContexts(parts, seperator = " ")
  }

  private def generateIfPart(withReturn: Boolean): GeneratorContext = {
    val head = ifParts.head
    generateIfContent(head, withReturn, (cond, body) => s"if ($cond) $body")
  }

  private def generateElseIfParts(withReturn: Boolean): GeneratorContext = {
    val elseIfs = ifParts.tail
    val contexts = elseIfs.map(e => generateIfContent(e, withReturn, (cond, body) => s"else if ($cond) $body"))
    GeneratorUtils.mergeGeneratorContexts(contexts, seperator = " ")
  }

  private def generateElsePart(withReturn: Boolean): GeneratorContext = {
    elseP match {
      case empty: EmptyExpression => GeneratorContext()
      case _ =>
        val els = generateCtx(elseP, withReturn)
        els.enhance(s"else ${els.content}")
    }
  }

  private def generateIfContent(ifPart: IfPart, withReturn: Boolean, f: (String, String) => String) = {
    val cond = ifPart.condition.generate
    val body = generateCtx(ifPart.body, withReturn)
    val content = f(cond.content, body.content)
    GeneratorUtils.mergeGeneratorContexts(Seq(cond, body), givenContent = content)
  }

  private def generateCtx(expression: Expression, withReturn: Boolean) = {
    if (withReturn) expression.generateReturn else expression.generate
  }

  override def skipSemiColon: Boolean = true
}

case class IfPart(condition: Expression, body: Expression)
