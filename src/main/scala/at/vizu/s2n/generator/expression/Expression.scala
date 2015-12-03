package at.vizu.s2n.generator
package expression

import at.vizu.s2n.types.TypeInference
import at.vizu.s2n.types.symbol._

import scala.reflect.api.InlineBlock
import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
  * Phil on 20.11.15.
  */

trait Expression {
  def exprTpe: TType

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
      val path: Path = generateApplyExpression(baseTypes, scope, a)
      ChainedExpression(path)
    case s: Select =>
      val path: Path = generateSelectExpression(baseTypes, scope, s)
      ChainedExpression(path)
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
    case i: If =>
      generateIfExpression(baseTypes, scope, i)
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
      case i: If => InlineBlock(wrapInBlock(i))
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

  def generateIfExpression(baseTypes: BaseTypes, scope: TScope, i: If) = {
    val (parts, elseExp) = generateIfParts(baseTypes, scope, i, Vector())
    IfExpression(baseTypes, scope, parts, elseExp)
  }

  def generateIfParts(baseTypes: BaseTypes, scope: TScope, i: If, parts: Vector[IfPart]): (Vector[IfPart], Expression) = {
    val condExp = Expression(baseTypes, scope, i.cond)
    val thenBlock = wrapInBlock(i.thenp)
    val thenExp = Expression(baseTypes, scope, thenBlock)
    val ifPart = IfPart(condExp, thenExp)
    i.elsep match {
      case ei: If => generateIfParts(baseTypes, scope, ei, parts :+ ifPart)
      case Literal(Constant(b: BoxedUnit)) => (parts, EmptyExpression(baseTypes.unit))
      case _ =>
        val elseBlock = wrapInBlock(i.elsep)
        (parts :+ ifPart, Expression(baseTypes, scope, elseBlock))
    }
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
        prevPath.dropRight(1) :+ newLast
      case Apply(i: Ident, _) =>
        Seq(generateIdentExpression(baseTypes, scope, i))
    }
  }

  private def generateSelectExpression(baseTypes: BaseTypes, scope: TScope, select: Select, method: Method = null): Path = {
    select match {
      case Select(s: Select, n) =>
        val prevPath: Path = generateSelectExpression(baseTypes, scope, s)
        val prevTpe = prevPath.last.exprTpe
        val member = if (method != null) method else TypeUtils.findMember(scope, n.toString, prevTpe)
        val elem = NestedExpression(prevTpe, "", member)
        prevPath :+ elem
      case Select(a: Apply, n) =>
        val prevPath = generateApplyExpression(baseTypes, scope, a)
        val prevTpe = prevPath.last.exprTpe
        val member = if (method != null) method else TypeUtils.findMember(scope, n.toString, prevTpe)
        val elem = NestedExpression(prevTpe, "", member)
        prevPath :+ elem
      case Select(l: Literal, n) =>
        val lTpe = TypeUtils.findType(scope, l)
        val member = if (method != null) method else TypeUtils.findMember(scope, n.toString, lTpe)
        Seq(NestedExpression(lTpe, l.value.value.toString, member))
      case Select(i: Ident, n) =>
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

  private def scoped(parentScope: TScope, f: TScope => Expression): Expression = {
    val childScope: TScope = parentScope.enterScope()
    val expr = f(childScope)
    childScope.exitScope()
    expr
  }

}