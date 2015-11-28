package at.vizu.s2n.generator
package expression

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

  def generateReturn: GeneratorContext = {
    val g = generate
    g.enhance("return " + g.content)
  }

  def skipSemiColon: Boolean
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
    case v: ValDef =>
      generateValDefExpression(baseTypes, scope, v)
    case a: Assign =>
      generateAssignExpression(baseTypes, scope, a)
    case l: LabelDef =>
      generateLabelDefExpression(baseTypes, scope, l)
    case EmptyTree =>
      println("Empty Tree!")
      EmptyExpression(baseTypes.unit)
  }

  def wrapInBlock(t: Tree): Block = {
    t match {
      case b: Block => b
      case _ => Block(Nil, t)
    }
  }

  def wrapInBlock(trees: List[Tree]): Block = {
    trees match {
      case expr :: Nil => Block(Nil, expr)
      case _ => Block(trees.dropRight(1), trees.last)
    }
  }

  def wrapInlineBlockIfBlock(t: Tree): Any = {
    t match {
      case b: Block => InlineBlock(b)
      case _ => t
    }
  }

  def generateLabelDefExpression(baseTypes: BaseTypes, scope: TScope, l: LabelDef) = l match {
    case LabelDef(n, _, If(cond, Block((body: Block) :: Nil, _), _)) =>
      val block: Block = wrapInBlock(body)
      generateWhileExpression(baseTypes, scope, block, cond)
    case LabelDef(n, _, If(cond, body: Block, _)) =>
      val block: Block = wrapInBlock(body.stats) // we are only taking stats since the expr is the recursive while method call
      generateWhileExpression(baseTypes, scope, block, cond)
    case LabelDef(n, _, Block((body: Block) :: Nil, If(cond, _, _))) =>
      generateDoWhileExpression(baseTypes, scope, body, cond)
    case LabelDef(n, _, Block(body, If(cond, _, _))) =>
      val block: Block = wrapInBlock(body)
      generateDoWhileExpression(baseTypes, scope, block, cond)
  }

  def generateWhileExpression(baseTypes: BaseTypes, scope: TScope, block: Block, cond: Tree) = {
    val blockExpr = getBlockExpression(baseTypes, scope, block) // we are only taking stats since the expr is the recursive while method call
    val condExpr = Expression(baseTypes, scope, cond)
    WhileExpression(baseTypes, condExpr, blockExpr)
  }

  def generateDoWhileExpression(baseTypes: BaseTypes, scope: TScope, block: Block, cond: Tree) = {
    val blockExpr = getBlockExpression(baseTypes, scope, block)
    val condExpr = Expression(baseTypes, scope, cond)
    DoWhileExpression(baseTypes, condExpr, blockExpr)
  }

  def generateValDefExpression(baseTypes: BaseTypes, scope: TScope, v: ValDef) = {
    val varTpe = findVarType(baseTypes, scope, v.tpt, v.rhs)
    val varName = v.name.toString
    TypeUtils.createIdentifier(scope, v, varTpe)
    val rhs = wrapInlineBlockIfBlock(v.rhs)
    val rhsExpr = Expression(baseTypes, scope, rhs)
    ValDefExpression(baseTypes, varName, rhsExpr)
  }

  def generateAssignExpression(baseTypes: BaseTypes, scope: TScope, a: Assign) = {
    val lhsExpr = Expression(baseTypes, scope, a.lhs)
    val block: Any = wrapInlineBlockIfBlock(a.rhs)
    val rhsExpr = getBlockExpression(baseTypes, scope, block, returnable = true)

    AssignExpression(lhsExpr, rhsExpr)
  }

  private def findVarType(baseTypes: BaseTypes, scope: TScope, tpt: Tree, rhs: Tree) = {
    val varTpe = TypeUtils.findType(scope, tpt)
    if (varTpe == null) TypeInference.getType(baseTypes, scope, rhs) else varTpe
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
      case i: Identifier => IdentExpression(i.tpe, s"$iName")
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
          val tpe = TypeInference.getType(baseTypes, scope, s.qualifier, argTypes)
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
        val identCtx = generateIdentExpression(baseTypes, scope, i).generate
        val tpe = TypeUtils.findIdentifier(scope, i).tpe
        val member = if (method != null) method else TypeUtils.findIdent(scope, n.toString, tpe)
        member match {
          case m: Method => Seq(NestedExpression(scope.findThis(), identCtx.content, m))
          case i: Identifier => Seq(NestedExpression(i.tpe, identCtx.content, TypeUtils.findMember(scope, n.toString, i.tpe)))
          case f: Field => Seq(NestedExpression(scope.findThis(), identCtx.content, f))
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

  override def skipSemiColon: Boolean = false
}

case class LiteralExpression(tpe: TType, literal: String) extends Expression {

  def prevTpe = tpe

  def generate = {
    s"$literal"
  }

  override def skipSemiColon: Boolean = false
}

case class NewExpression(baseTypes: BaseTypes, tpe: TType, params: Seq[Expression] = Seq()) extends Expression {
  def prevTpe = tpe

  def generate = {
    val cppName = GeneratorUtils.getCppTypeName(baseTypes, tpe)
    val paramsContext = GeneratorUtils.mergeGeneratorContexts(params.map(_.generate), ", ")
    val sharedPtrName = GeneratorUtils.generateCppTypeName(baseTypes, tpe)
    paramsContext.enhance(s"$sharedPtrName(new $cppName(${paramsContext.content}))")
  }

  override def skipSemiColon: Boolean = false
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

  override def skipSemiColon: Boolean = true
}

abstract class BaseBlockExpression(stats: List[Expression], expr: Expression, returnable: Boolean = false) extends Expression {
  override def prevTpe: TType = expr.prevTpe

  override def generate: GeneratorContext = {
    val statsStats = generateExpressionChain(stats, "\n")
    val bodyCtxList = Seq(statsStats, generateExpr())
    val bodyCtx: GeneratorContext = GeneratorUtils.mergeGeneratorContexts(bodyCtxList, "\n  ")
    generateContentStr(bodyCtx)
  }

  protected def generateContentStr(bodyCtx: GeneratorContext): GeneratorContext

  private def generateExpr() = {
    val exprGenerate: GeneratorContext = expr.generate
    val exprContent: String = exprGenerate.content
    exprGenerate.enhance(generateExprString(exprContent, returnable, expr.skipSemiColon))
  }

  private def generateExprString(exprContent: String, returnable: Boolean, skipSemicolon: Boolean) = {
    val exprString1 = if (returnable) "return " + exprContent else exprContent
    if (skipSemiColon) exprString1 else exprString1
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

  override def skipSemiColon: Boolean = true
}

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

case class InlineDefExpression(baseTypes: BaseTypes, method: Method, body: BaseBlockExpression) extends Expression {
  override def prevTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val bodyCtx: GeneratorContext = body.generate
    val paramsString = GeneratorUtils.generateParamsString(baseTypes, method.params, withVars = true)
    val contentStr = s"auto ${method.name} = [&]($paramsString) ${bodyCtx.content}"
    bodyCtx.enhance(contentStr)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}

case class ConstructorExpression(baseTypes: BaseTypes, method: Method, initMethodName: String) extends Expression {
  override def prevTpe: TType = method.returnType

  override def generate: GeneratorContext = {
    val typeName = GeneratorUtils.getCppTypeName(prevTpe.pkg, prevTpe.simpleName)
    val paramsString: String = GeneratorUtils.generateParamsString(baseTypes, method.params, withVars = true)
    val initMethodCall = s"this->$initMethodName()"
    val expressions: Seq[Expression] = (method.params.map(p => s"this->${p.name} = ${p.name}") :+ initMethodCall
      ).map(LiteralExpression(baseTypes.unit, _))
    val ctx: GeneratorContext = generateExpressionChain(expressions, "\n")
    val content: String =
      s"""$typeName::${prevTpe.simpleName}($paramsString) {
         |  ${ctx.content}
         |}""".stripMargin
    ctx.enhance(content)
  }

  override def skipSemiColon: Boolean = true
}

case class ValDefExpression(baseTypes: BaseTypes, varName: String, rhs: Expression) extends Expression {
  override def prevTpe: TType = null

  override def generate: GeneratorContext = {
    val varTpe = rhs.prevTpe
    val lhs = s"${GeneratorUtils.generateCppTypeName(baseTypes, varTpe)} $varName"
    val rhsCtx = rhs.generate
    val defString = s"$lhs = ${rhsCtx.content}"
    rhsCtx.enhance(defString)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}

case class AssignExpression(lhs: Expression, rhs: Expression) extends Expression {
  override def prevTpe: TType = null

  override def generate: GeneratorContext = {
    val lhsCtx = lhs.generate
    val rhsCtx = rhs.generate
    val generated = s"${lhsCtx.content} = ${rhsCtx.content}"
    GeneratorUtils.mergeGeneratorContexts(Seq(lhsCtx, rhsCtx), givenContent = generated)
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate
}

case class WhileExpression(baseTypes: BaseTypes, condExpr: Expression, body: Expression) extends Expression {
  override def prevTpe: TType = baseTypes.unit

  override def generate: GeneratorContext = {
    val condCtx = condExpr.generate
    val bodyCtx = body.generate
    val content = s"while(${condCtx.content}) ${bodyCtx.content}"
    GeneratorUtils.mergeGeneratorContexts(Seq(condCtx, bodyCtx), givenContent = content)
  }

  override def skipSemiColon: Boolean = true

  override def generateReturn: GeneratorContext = generate
}

case class DoWhileExpression(baseTypes: BaseTypes, condExpr: Expression, body: Expression) extends Expression {
  override def prevTpe: TType = baseTypes.unit

  override def generate: GeneratorContext = {
    val condCtx = condExpr.generate
    val bodyCtx = body.generate
    val content = s"do ${bodyCtx.content} while (${condCtx.content})"
    GeneratorUtils.mergeGeneratorContexts(Seq(condCtx, bodyCtx), givenContent = content)
  }

  override def skipSemiColon: Boolean = true

  override def generateReturn: GeneratorContext = generate
}

case class IfExpression(baseTypes: BaseTypes, condExpr: Expression, thenP: Expression, elseP: Expression) extends Expression {
  override def prevTpe: TType = ???

  override def generate: GeneratorContext = ???

  override def skipSemiColon: Boolean = ???
}

case class ChainedExpression(path: Path) extends Expression {
  override def prevTpe: TType = path.last.prevTpe

  override def generate: GeneratorContext = generateExpressionChain(path)

  override def skipSemiColon: Boolean = false
}

case class EmptyExpression(unit: TType) extends Expression {
  override def prevTpe: TType = unit

  override def generate: GeneratorContext = GeneratorContext()

  override def skipSemiColon: Boolean = true

  override def generateReturn: GeneratorContext = generate
}