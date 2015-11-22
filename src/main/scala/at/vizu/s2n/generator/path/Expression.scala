package at.vizu.s2n.generator
package path

import at.vizu.s2n.generator._
import at.vizu.s2n.scala.reflect.api.InlineBlock
import at.vizu.s2n.types.TypeInference
import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._

/**
  * Phil on 20.11.15.
  */

trait Expression {
  def prevTpe: TType

  def generate: GeneratorContext
}

object Expression {

  def apply(baseTypes: BaseTypes, scope: TScope, t: Any): Expression = t match {
    case l: Literal =>
      val tpe = TypeUtils.findType(scope, l)
      LiteralExpression(tpe, l.value.value.toString)
    case a: Apply =>
      ChainedExpression(generateApplyExpression(baseTypes, scope, a))
    case s: Select =>
      ChainedExpression(generateSelectExpression(baseTypes, scope, s))
    case i: Ident =>
      generateIdentExpression(baseTypes, scope, i)
    case InlineBlock(_) | Block(_, _) =>
      getBlockExpression(baseTypes, scope, t)
    case d: DefDef =>
      generateInlineDefExpression(baseTypes, scope, d)
  }

  def wrapInBlock(t: Tree): Block = {
    t match {
      case b: Block => b
      case _ => Block(Nil, t)
    }
  }

  def generateInlineDefExpression(baseTypes: BaseTypes, scope: TScope, d: DefDef) = {
    scoped(scope, (childScope: TScope) => {
      val nestedMethod: Method = TypeUtils.createMethod(scope, d, instanceMethod = false)
      TypeUtils.addParamsToScope(childScope, nestedMethod.params)
      scope.addMethod(nestedMethod)
      val block = wrapInBlock(d.rhs)
      val blockExpr = getBlockExpression(baseTypes, childScope, block, returnable = true)
      InlineDefExpression(baseTypes, nestedMethod, blockExpr)
    })
  }

  def getBlockExpression(baseTypes: BaseTypes, scope: TScope, t: Any, returnable: Boolean = false) = {
    t match {
      case ib: InlineBlock =>
        val (stats, expr) = getBlockContent(baseTypes, scope, ib.stats, ib.expr)
        InlineBlockExpression(stats, expr, returnable)
      case b: Block =>
        val (stats, expr) = getBlockContent(baseTypes, scope, b.stats, b.expr)
        BlockExpression(stats, expr, returnable)
    }
  }

  def getBlockContent(baseTypes: BaseTypes, scope: TScope, statsT: List[Tree], exprT: Tree): (List[Expression], Expression) = {
    val stats: List[Expression] = statsT.map(Expression(baseTypes, scope, _))
    val expr = Expression(baseTypes, scope, exprT)
    (stats, expr)
  }

  private def generateIdentExpression(baseTypes: BaseTypes, scope: TScope, ident: Ident): Expression = {
    val iName: String = ident.name.toString
    TypeUtils.findIdent(scope, iName.toString) match {
      case m: Method =>
        if (m.instanceMethod) IdentExpression(m.returnType, s"this->$iName()")
        else IdentExpression(m.returnType, s"$iName()")
      case i: Identifier => IdentExpression(i.tpe, s"$iName()")
      case f: Field => IdentExpression(f.tpe, s"this->$iName")
      case _ => throw new RuntimeException("TODO")
    }
  }

  private def generateApplyExpression(baseTypes: BaseTypes, scope: TScope, apply: Apply): Path = {
    apply match {
      case Apply(Select(ne: New, n), pList) =>
        val tpe: TType = TypeUtils.findType(scope, ne)
        val params: Seq[Expression] = apply.args.map(arg => Expression(baseTypes, scope, arg))
        Seq(NewExpression(baseTypes, tpe, params))
      case Apply(s: Select, pList) =>
        val params: Seq[Expression] = apply.args.map(arg => Expression(baseTypes, scope, arg))
        val prevPath: Path = if (params.nonEmpty) {
          val argTypes: List[TType] = TypeInference.getTypes(baseTypes, scope, apply.args)
          val tpe = TypeInference.getType(baseTypes, scope, s, argTypes)
          val method: Method = TypeUtils.findMethod(scope, s.name.toString, s.pos.line, argTypes, tpe)
          generateSelectExpression(baseTypes, scope, s, method)
        } else {
          generateSelectExpression(baseTypes, scope, s)
        }
        val last: NestedExpression = prevPath.last.asInstanceOf[NestedExpression]

        val newLast: NestedExpression = last.copy(params = params)
        prevPath.drop(1) :+ newLast
      case Apply(i: Ident, _) =>
        Seq(generateIdentExpression(baseTypes, scope, i))
    }
  }

  private def generateSelectExpression(baseTypes: BaseTypes, scope: TScope, select: Select, method: Method = null): Path = {
    select match {
      case Select(s: Select, n) =>
        val prevPath: Path = generateSelectExpression(baseTypes, scope, s)
        val tpe = getType(prevPath.last.prevTpe)
        val member: Modifiable = if (method != null) method else TypeUtils.findMember(scope, n.toString, tpe)
        val elem = NestedExpression(tpe, "", member)
        prevPath :+ elem
      case Select(a: Apply, n) =>
        generateApplyExpression(baseTypes, scope, a)
      case Select(l: Literal, n) =>
        val lTpe = TypeUtils.findType(scope, l)
        val member: Modifiable = if (method != null) method else TypeUtils.findMember(scope, n.toString, lTpe)
        Seq(NestedExpression(lTpe, l.value.value.toString, member))
      case Select(i: Ident, n) =>
        val iName: String = i.name.toString
        val tpe = TypeUtils.findIdentifier(scope, i).tpe
        val member = if (method != null) method else TypeUtils.findIdent(scope, n.toString, tpe)
        member match {
          case m: Method => Seq(NestedExpression(scope.findThis(), s"this->$iName", m))
          case i: Identifier => Seq(NestedExpression(i.tpe, iName, TypeUtils.findMember(scope, n.toString, i.tpe)))
          case f: Field => Seq(NestedExpression(scope.findThis(), s"this->$iName", f))
          case _ => throw new RuntimeException("Todo")
        }
      case Select(t: This, n) =>
        Seq(NestedExpression(scope.findThis(), "this", TypeUtils.findMember(scope, n.toString)))
    }
  }

  private def getType(modifiable: Modifiable): TType = {
    modifiable match {
      case m: Method => m.returnType
      case f: Field => f.tpe
    }
  }

  private def scoped(parentScope: TScope, f: TScope => Expression): Expression = {
    val childScope: TScope = parentScope.enterScope()
    val expr = f(childScope)
    childScope.exitScope()
    expr
  }

}

case class IdentExpression(tpe: TType, expr: String) extends Expression {
  def prevTpe = tpe

  def generate = {
    expr
  }
}

case class LiteralExpression(tpe: TType, literal: String) extends Expression {

  def prevTpe = tpe

  def generate = {
    s"$literal"
  }
}

case class NewExpression(baseTypes: BaseTypes, tpe: TType, params: Seq[Expression] = Seq()) extends Expression {
  def prevTpe = tpe

  def generate = {
    val cppName = GeneratorUtils.getCppTypeName(baseTypes, tpe)
    val paramsContext = GeneratorUtils.mergeGeneratorContexts(params.map(_.generate), ", ")
    val sharedPtrName = GeneratorUtils.generateCppTypeName(baseTypes, tpe)
    paramsContext.enhance(s"$sharedPtrName(new $cppName(${paramsContext.content}))")
  }
}

case class NestedExpression(tpe: TType, varName: String, member: Modifiable, params: Seq[Expression] = Seq()) extends Expression {
  def prevTpe = tpe

  def generate: GeneratorContext = {
    member match {
      case f: Field => generateFieldCallOnType(tpe, varName, f)
      case m: Method => generateMethodCallOnType(tpe, varName, m, params)
    }
  }

  private def generateMethodCallOnType(tpe: TType, varName: String, m: Method, params: Seq[Expression]): GeneratorContext = {
    val paramsContext = GeneratorUtils.mergeGeneratorContexts(params.map(_.generate), ", ")
    val paramsContent: String = paramsContext.content
    if (isOperator()) {
      val prettyOperator = m.name
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

  private def isOperator(): Boolean = false

  private def isNonPointerCall(): Boolean = false
}

abstract class BaseBlockExpression(stats: List[Expression], expr: Expression, returnable: Boolean = false) extends Expression {
  override def prevTpe: TType = expr.prevTpe

  override def generate: GeneratorContext = {
    val statsStats = generateExpressionChain(stats, ";\n")
    val bodyCtxList = Seq(statsStats, generateExpr())
    val bodyCtx: GeneratorContext = GeneratorUtils.mergeGeneratorContexts(bodyCtxList, ";\n")
    generateContentStr(bodyCtx)
  }

  protected def generateContentStr(bodyCtx: GeneratorContext): GeneratorContext

  private def generateExpr() = {
    val exprGenerate: GeneratorContext = expr.generate
    val exprContent: String = exprGenerate.content
    if (returnable) exprGenerate.enhance("return " + exprContent + ";") else exprGenerate.enhance(exprContent + ";")
  }

}

case class BlockExpression(stats: List[Expression], expr: Expression, returnable: Boolean = false)
  extends BaseBlockExpression(stats, expr, returnable) {
  protected def generateContentStr(bodyCtx: GeneratorContext) = {
    val contentStr =
      s"""{
          |  ${bodyCtx.content}
          |}""".stripMargin
    bodyCtx.enhance(contentStr)
  }
}

case class InlineBlockExpression(stats: List[Expression], expr: Expression, returnable: Boolean = false)
  extends BaseBlockExpression(stats, expr, returnable) {
  protected def generateContentStr(bodyCtx: GeneratorContext) = {
    val contentStr =
      s"""[this,&]() {
          |  ${bodyCtx.content}
          |}()""".stripMargin
    bodyCtx.enhance(contentStr)
  }
}

case class InlineDefExpression(baseTypes: BaseTypes, method: Method, body: BaseBlockExpression) extends Expression {
  override def prevTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val bodyCtx: GeneratorContext = body.generate
    val paramsString = GeneratorUtils.generateParamsString(baseTypes, method.params, withVars = true)
    val contentStr = s"auto ${method.name} = [this,&]($paramsString) ${bodyCtx.content};"
    bodyCtx.enhance(contentStr)
  }
}

case class ChainedExpression(path: Path) extends Expression {
  override def prevTpe: TType = path.last.prevTpe

  override def generate: GeneratorContext = generateExpressionChain(path)
}