package at.vizu.s2n.types

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.types.symbol.{BaseTypes, TScope, TType, TypeUtils}

import scala.reflect.runtime.universe._

/**
  * Phil on 20.11.15.
  */
object TypeInference {

  def getTypes(baseTypes: BaseTypes, scope: TScope, trees: List[Tree]) = {
    trees.map(getType(baseTypes, scope, _))
  }

  def getType(baseTypes: BaseTypes, scope: TScope, tree: Tree, params: Seq[TType] = Vector()): TType = {
    tree match {
      case b: Block => getTypeBlock(baseTypes, scope, b)
      case v: ValDef => baseTypes.unit
      case d: DefDef => baseTypes.unit
      case a: Assign => baseTypes.unit
      case a: Apply => getTypeApply(baseTypes, scope, a)
      case s: Select => getTypeSelect(baseTypes, scope, s, params)
      case i: Ident => getTypeIdent(baseTypes, scope, i)
      case l: Literal => getTypeLiteral(baseTypes, scope, l)
      case i: If => getTypeIf(baseTypes, scope, i)
      case l: LabelDef => baseTypes.unit
      case f: Function => throw new TypeException(scope.currentFile, tree.pos.line, "Anonymous functions are currently not supported")
      case EmptyTree => throw new TypeException(scope.currentFile, tree.pos.line, "EmptyTree")
    }
  }

  def getTypeBlock(baseTypes: BaseTypes, scope: TScope, block: Block): TType = {
    getType(baseTypes, scope, block.expr)
  }

  def getTypeSelect(baseTypes: BaseTypes, scope: TScope, select: Select, params: Seq[TType] = Vector()): TType = {
    val tpe: TType = getType(baseTypes, scope, select.qualifier)
    val selectName: String = select.name.toString

    if (params.isEmpty) TypeUtils.findMethodOrFieldType(scope, selectName, select.pos.line, tpe)
    else TypeUtils.findMethod(scope, selectName, select.pos.line, params, tpe).returnType
  }

  def getTypeApply(baseTypes: BaseTypes, scope: TScope, apply: Apply): TType = {
    apply.fun match {
      case s: Select =>
        val tpe: TType = getType(baseTypes, scope, s.qualifier)
        val args: Seq[TType] = getTypes(baseTypes, scope, apply.args)
        val selectName: String = s.name.toString

        TypeUtils.findMethod(scope, selectName, s.pos.line, args, tpe).returnType
      case i: Ident =>
        getTypeIdent(baseTypes, scope, i)
    }
  }

  def getTypeIdent(baseTypes: BaseTypes, scope: TScope, ident: Ident): TType = {
    val iName = ident.name.toString
    scope.findMethod(iName, Vector()) match {
      case Some(m) => m.returnType
      case None =>
        scope.findIdentifier(iName) match {
          case Some(i) => i.tpe
          case None =>
            val thisTpe: TType = scope.findThis()
            thisTpe.findMethod(scope.findThis(), iName, Vector()) match {
              case Some(tMethod) => tMethod.returnType
              case None =>
                thisTpe.findField(thisTpe, iName) match {
                  case Some(field) => field.tpe
                  case None => throw new RuntimeException("TODO")
                }
            }
        }
    }
  }

  def getTypeLiteral(baseTypes: BaseTypes, scope: TScope, literal: Literal): TType = {
    TypeUtils.findType(scope, literal)
  }

  def getTypeIf(baseTypes: BaseTypes, scope: TScope, i: If): TType = {
    val elseType = i.elsep match {
      case l: Literal => getTypeLiteral(baseTypes, scope, l)
      case _ => getType(baseTypes, scope, i.elsep)
    }
    val thenType = getType(baseTypes, scope, i.thenp)
    TypeUtils.findCommonBaseClass(scope, thenType, elseType)
  }

}
