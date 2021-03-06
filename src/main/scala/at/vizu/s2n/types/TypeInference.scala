package at.vizu.s2n.types

import at.vizu.s2n.error.TypeErrors
import at.vizu.s2n.log.Profiler.profile
import at.vizu.s2n.log.Trace
import at.vizu.s2n.types.symbol._
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.runtime.universe._

/**
  * Phil on 20.11.15.
  */
object TypeInference extends LazyLogging {

  def getTypes(baseTypes: BaseTypes, scope: TSymbolTable, trees: List[Tree]) = {
    profile(logger, "TypeInference.getTypes", getTypesInternal(baseTypes, scope, trees), Trace)
  }

  private def getTypesInternal(baseTypes: BaseTypes, scope: TSymbolTable, trees: List[Tree]) = {
    trees.map(getTypeInternal(baseTypes, scope, _))
  }

  def getType(baseTypes: BaseTypes, scope: TSymbolTable, tree: Tree, params: Seq[TType] = Vector()) = {
    profile(logger, "TypeInference.getType", getTypeInternal(baseTypes, scope, tree, params), Trace)
  }

  def getTypeInternal(baseTypes: BaseTypes, scope: TSymbolTable, tree: Tree, params: Seq[TType] = Vector()): TType = {
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
      case f: Function => getTypeFunction(scope, f) //TypeErrors.addError(scope, tree.pos.line, "Anonymous functions are currently not supported")
      case t: This => scope.findThis()
      case EmptyTree => TypeErrors.addError(scope, tree.pos.line, "EmptyTree")
    }
  }

  def getTypeBlock(baseTypes: BaseTypes, scope: TSymbolTable, block: Block): TType = {
    getTypeInternal(baseTypes, scope, block.expr)
  }

  def getTypeSelect(baseTypes: BaseTypes, scope: TSymbolTable, select: Select, params: Seq[TType] = Vector()): TType = {
    val tpe: TType = getTypeInternal(baseTypes, scope, select.qualifier)
    val selectName: String = select.name.toString

    if (params.isEmpty) TypeUtils.findMethodOrFieldType(scope, selectName, select.pos.line, tpe)
    else TypeUtils.findMethod(scope, selectName, select.pos.line, params, tpe).returnType
  }

  def getTypeApply(baseTypes: BaseTypes, scope: TSymbolTable, apply: Apply): TType = {
    apply.fun match {
      case s@Select(New(a: AppliedTypeTree), name) =>
        val onType: TType = TypeUtils.findType(scope, a.tpt)
        val appliedTypes: List[TType] = a.args.map(TypeUtils.findType(scope, _))
        val line: Int = apply.pos.line
        val newType = TypeUtils.applyTypesOnType(scope, onType, appliedTypes, line)
        val args = getTypesInternal(baseTypes, scope, apply.args)
        TypeUtils.findConstructor(scope, line, args, newType).returnType
      case s@Select(n: New, name) =>
        val args = getTypesInternal(baseTypes, scope, apply.args)
        TypeUtils.applyConstructor(scope, args, n)
      case s: Select =>
        val tpe: TType = getTypeInternal(baseTypes, scope, s.qualifier)
        val args: Seq[TType] = getTypesInternal(baseTypes, scope, apply.args)
        val selectName: String = s.name.toString

        val method: Method = TypeUtils.findMethod(scope, selectName, s.pos.line, args, tpe)
        if (method.generics.nonEmpty) {
          val newMethod = method.applyTypes(scope, method.getAppliedTypes(args))
          newMethod.returnType
        } else method.returnType
      case i: Ident =>
        val args = getTypesInternal(baseTypes, scope, apply.args)
        getTypeIdent(baseTypes, scope, i, args)
    }
  }

  def getTypeIdent(baseTypes: BaseTypes, scope: TSymbolTable, ident: Ident, args: Seq[TType] = Vector()): TType = {
    val iName = ident.name.toString
    profile(logger, "ti: ident - findmethod", scope.findMethod(iName, args), Trace) match {
      case Some(m) =>
        if (m.generics.nonEmpty) {
          val appliedTypes = m.getAppliedTypes(args)
          m.applyTypes(scope, appliedTypes).returnType
        } else m.returnType
      case None =>
        profile(logger, "ti: ident - findidentifier", scope.findIdentifier(iName), Trace) match {
          case Some(i) => i.tpe
          case None =>
            val thisTpe: TType = scope.findThis()
            thisTpe.findMethod(scope.findThis(), iName, args) match {
              case Some(tMethod) =>
                if (tMethod.generics.nonEmpty) {
                  val appliedTypes = tMethod.getAppliedTypes(args)
                  tMethod.applyTypes(scope, appliedTypes).returnType
                } else tMethod.returnType
              case None =>
                thisTpe.findField(thisTpe, iName) match {
                  case Some(field) => field.tpe
                  case None =>
                    scope.findObject(iName) match {
                      case Some(obj) => obj
                      case None => throw new RuntimeException("TODO")
                    }
                }
            }
        }
    }
  }

  def getTypeLiteral(baseTypes: BaseTypes, scope: TSymbolTable, literal: Literal): TType = {
    TypeUtils.findType(scope, literal)
  }

  def getTypeIf(baseTypes: BaseTypes, scope: TSymbolTable, i: If): TType = {
    val elseType = i.elsep match {
      case l: Literal => getTypeLiteral(baseTypes, scope, l)
      case _ => getTypeInternal(baseTypes, scope, i.elsep)
    }
    val thenType = getTypeInternal(baseTypes, scope, i.thenp)
    TypeUtils.findCommonBaseClass(scope, thenType, elseType)
  }

  def getTypeFunction(scope: TSymbolTable, f: Function): TType = {
    scope.scoped((childScope: TSymbolTable) => {
      val params: Seq[Param] = TypeUtils.createParams(childScope, f.vparams)
      TypeUtils.addParamsToScope(childScope, params)
      val retType = getTypeInternal(childScope.baseTypes, childScope, f.body)
      TypeUtils.createFunctionTypeFromParams(childScope, params, retType, f.pos.line)
    }, MethodScope)
  }

  def getTypeNew(baseTypes: BaseTypes, scope: TSymbolTable, n: New): TType = {
    TypeUtils.findType(scope, n)
  }
}
