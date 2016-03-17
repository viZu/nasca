package at.vizu.s2n.generator.expression

import at.vizu.s2n.conf.GlobalConfig
import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol._

/**
  * Phil on 29.11.15.
  */
case class NestedExpression(baseTypes: BaseTypes, scope: TSymbolTable, prevTpe: TType, varName: String,
                            member: Member, params: Seq[Expression] = Vector()) extends Expression {
  lazy val exprTpe = member.tpe

  def generate: GeneratorContext = {
    member match {
      case f: Field => generateFieldCallOnType(prevTpe, varName, f)
      case m: Method if m.instanceMethod => generateMethodCallOnType(prevTpe, varName, m)
      case m: Method => generateMethodCall(m)
    }
  }

  private def generateMethodCall(m: Method): GeneratorContext = {
    val paramsContext = GeneratorUtils.mergeGeneratorContexts(params.map(_.content), ", ")
    if (hasInvocationHandle) {
      val callCtx = executeInvocationHandle()
      GeneratorUtils.mergeGeneratorContexts(Vector(paramsContext, callCtx), givenContent = callCtx.value)
    } else {
      throw new RuntimeException("TODO")
    }
  }

  private def generateMethodCallOnType(tpe: TType, varName: String, m: Method): GeneratorContext = {
    val paramsContext = generateParamsContext(m)
    val paramsContent: String = paramsContext.value
    if (hasInvocationHandle) {
      val callCtx = executeInvocationHandle()
      val content = varName + callCtx
      GeneratorUtils.mergeGeneratorContexts(Vector(paramsContext, callCtx), givenContent = content)
    } else if (isUnaryOperator(m)) {
      val prettyOperator = prettifyUnaryOperator(m.name)
      paramsContext.enhance(s"$prettyOperator$varName")
    } else if (isOperator(m)) {
      val prettyOperator = prettifyOperator(m.name)
      paramsContext.enhance(s"$varName $prettyOperator $paramsContent")
    } else if (isNonPointerCall(m)) {
      val mName = GeneratorUtils.prettifyMethod(m.name)
      paramsContext.enhance(s"$varName.$mName($paramsContent)")
    } else {
      val call = if (tpe.isObject) s"$varName->getInstance()->" else s"$varName->"
      val typeParams: GeneratorContext = if (m.generics.nonEmpty) GeneratorUtils.generateTypeArgs(baseTypes, m.generics) else ""
      val mName = GeneratorUtils.prettifyMethod(m.name)
      paramsContext.enhance(s"$call$mName$typeParams($paramsContent)", typeParams.handles)
    }
  }

  private def generateParamsContext(m: Method) = {
    val ctxSeq: Seq[GeneratorContext] = m.params.map(_.tpe).zip(params).map({
      case (tpe, expr) if TypeUtils.isFunctionType(tpe) =>
        val ctx = expr.content
        ctx.enhance(ctx.value.replaceAll("\\(\\)", ""))
      case (tpe, expr) => expr.content
    })
    GeneratorUtils.mergeGeneratorContexts(ctxSeq, ", ")
  }

  private def generateFieldCallOnType(tpe: TType, varName: String, f: Field): GeneratorContext = {
    val call = if (isNonPointerCall) "." else "->"
    val methodName = GeneratorUtils.generateFieldAccessor(f)
    s"$varName$call$methodName"
  }

  private def hasInvocationHandle: Boolean = {
    val paramTypes = member match {
      case m: Method => m.params.map(_.tpe)
      case f: Field => Vector()
    }
    val tpeName = if (prevTpe != null) prevTpe.name else ""
    GlobalConfig.invocationConfig.hasInvocationHandle(scope, tpeName, member.name, paramTypes)
  }

  private def executeInvocationHandle(): GeneratorContext = {
    val paramTypes = member match {
      case m: Method => m.params.map(_.tpe)
      case f: Field => Vector()
    }
    val handle = GlobalConfig.invocationConfig.findInvocationHandle(scope, prevTpe.name, member.name, paramTypes)
    val paramsAsString = params.map(_.content.value)
    handle(varName, paramsAsString)
  }

  private def isOperator(m: Method): Boolean = m.operator

  private def isUnaryOperator(m: Method): Boolean = m.operator && m.name.startsWith("unary_")

  private def isNonPointerCall(m: Method): Boolean = m.nonPointer

  private def isNonPointerCall: Boolean = false

  override def skipSemiColon: Boolean = true
}
