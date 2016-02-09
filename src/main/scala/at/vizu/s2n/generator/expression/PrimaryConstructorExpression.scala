package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.symbol._

/**
  * Phil on 29.11.15.
  */
case class PrimaryConstructorExpression(scope: TScope, constructor: Constructor, initMethodName: String)
  extends ConstructorExpression {

  override def exprTpe: TType = constructor.returnType

  private def generatePrimary: GeneratorContext = {
    val typeName = GeneratorUtils.getCppTypeName(scope.baseTypes, exprTpe) //GeneratorUtils.getCppTypeName(exprTpe.pkg, exprTpe.simpleName, "")
    val paramsString = GeneratorUtils.generateParamsString(scope.baseTypes, constructor.params, withVars = true)
    val expressions: Seq[Expression] = generateInitMethodExpr()
    val initializer = generateInitializer()
    val ctx: GeneratorContext = generateExpressionChain(expressions, "\n")
    val templateString = GeneratorUtils.generateClassTemplate(exprTpe)
    val content: String =
      s"""$templateString$typeName::${exprTpe.simpleName}($paramsString) $initializer{
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content, paramsString.handles)
  }

  private def generateInitializer(): String = {
    val strings: Seq[String] = constructor.params.map(p => s"${p.name}(${p.name})") :+ generateSuperInit()
    val initializers = strings.filter(_.nonEmpty).mkString(", ")
    if (initializers.nonEmpty) s": $initializers " else ""
  }

  private def generateSuperInit(): String = {
    val tpe: TType = scope.findThis()
    tpe.parents.find(_.expr.nonEmpty).map(generateSuperInitString) getOrElse ""
  }

  private def generateSuperInitString(parent: Parent): String = {
    val typeName = GeneratorUtils.getCppTypeName(scope.baseTypes, parent.tpe).content
    val params: String = parent.expr.map(_.generate.content).mkString(", ")
    s"$typeName($params)"
  }

  private def generateInitMethodExpr() = {
    if (initMethodName.isEmpty) Vector()
    else {
      val initMethodCall = s"this->$initMethodName()"
      Vector(LiteralExpression(scope.baseTypes.unit, initMethodCall))
    }
  }

  override def skipSemiColon: Boolean = true

  override def paramsString: GeneratorContext = {
    GeneratorUtils.generateParamsString(scope.baseTypes, constructor.params, withVars = true)
  }

  override def bodyContent: Seq[Expression] = generateInitMethodExpr()

  override def initializer: String = generateInitializer()
}
