package at.vizu.s2n.types

import at.vizu.s2n.error.TypeErrors
import at.vizu.s2n.log.Debug
import at.vizu.s2n.log.Profiler._
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.result._
import at.vizu.s2n.types.symbol._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 23.10.15.
 */
class ReflectTypeChecker(baseTypes: BaseTypes) extends TypeChecker with LazyLogging {

  def checkTypes(rootScope: TScope, tree: AST): ScalaFileWrapper = {
    require(tree != null)
    profileFunc(logger, s"Type checker ${tree.fileName}", () => {
      val traverser: ClassTraverser = new ClassTraverser(rootScope, tree)
      traverser.traverse(tree.internalTree)
      traverser.createFileWrapper()
    }, Debug)
  }

  private class ClassTraverser(var currentScope: TScope, ast: AST) extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    val impls = new ArrayBuffer[Implementation]
    val imports = new ArrayBuffer[ImportStmt]
    var scoped: Boolean = false

    override def traverse(tree: Tree): Unit = {
      tree match {
        case c: ImplDef =>
          handleEnterChildScope()
          val thisTpe: TType = TypeUtils.findType(currentScope, c)
          checkImplementation(currentScope.enterScope(thisTpe), c)
          impls += getImplementation(c, thisTpe)
        case PackageDef(Ident(name), subtree) =>
          pkgBuilder.append(name.toString)
          super.traverse(tree)
        case i: Import =>
          handleEnterChildScope()
          handleImport(i)
        //imports += i.toString()
        case _ => super.traverse(tree)
      }
    }

    def packageName = {
      pkgBuilder.mkString(".")
    }

    private def handleImport(i: Import) = {
      i.selectors.foreach(s => addTypeAlias(i.expr.toString(), s, i.pos.line))
    }

    private def addTypeAlias(pkgName: String, selector: ImportSelector, line: Int): Unit = {
      val typeName: String = pkgName + "." + selector.name.toString
      currentScope.findClass(typeName)
        .orElse(currentScope.findObject(typeName))
        .getOrElse(TypeErrors.addError(currentScope, line, s"No type with name $typeName found"))
      currentScope.addTypeAlias(selector.rename.toString, typeName)
      imports += ImportStmt(pkgName, selector.name.toString, selector.rename.toString)
    }

    private def handleEnterChildScope() = {
      if (!scoped) {
        currentScope = currentScope.enterScope(BlockScope)
        currentScope.currentFile = ast.fileName
        currentScope.currentPackage = packageName
        scoped = true
      }
    }

    private def getImplementation(implDef: ImplDef, tpe: TType) = {
      implDef match {
        case c: ClassDef => ClassImplementation(c, tpe)
        case m: ModuleDef => ObjectImplementation(m, tpe)
      }
    }

    def createFileWrapper() = {
      ScalaFileWrapper(impls, imports, packageName)
    }
  }

  private def checkImplementation(scope: TScope, c: ImplDef) = {
    checkGenerics(scope)
    checkMembers(scope, c.impl)
    checkMembers(scope, c.impl)
  }

  private def checkGenerics(scope: TScope) = {
    scope.findThis() match {
      case gt: GenericType =>
        // TODO check generic for unique identifier
        gt.genericModifiers.foreach(scope.addClass)
      case _ =>
    }
  }

  private def checkMembers(scope: TScope, t: Template) = {
    t.body.foreach(m => checkMember(scope, m))
  }

  private def checkMember(scope: TScope, member: Tree) = {
    member match {
      case v: ValDef => checkValMember(scope, v)
      case d: DefDef => checkDefMember(scope, d)
      case a: Apply => checkApply(scope, a)
      case s: Select => checkSelect(scope, s)
      case _ => TypeErrors.addError(scope, member.pos.line, s"unrecognized member ${member.toString()}")
    }
  }

  private def checkValMember(scope: TScope, v: ValDef) = {
    val field: Field = TypeUtils.findField(scope, v)
    if (!field.isAbstract) {
      scoped(scope, (s: TScope) => {
        val returnType: Option[TType] = checkValOrDefBody(s, v.rhs, field.tpe)
        returnType match {
          case Some(tpe) => if (field.tpe == null) field.tpe = tpe
          case None => if (field.tpe == null) TypeErrors.addError(scope, v.pos.line,
            s"Value definition ${v.name} requires a valid return type")
        }
      })
    }
  }

  private def checkDefMember(scope: TScope, d: DefDef) = {
    scoped(scope, (s: TScope) => {
      TypeUtils.createAndAddGenericModifiers(s, d.tparams)
      val method: Method = TypeUtils.findMethodForDef(s, d)
      if (!method.constructor && !method.isAbstract) {
        val expected: TType = method.returnType
        TypeUtils.addParamsToScope(s, method.params)
        checkValOrDefBody(s, d.rhs, expected)
      }
    })
  }

  private def checkReturnExpected(returned: Option[TType], expected: TType, line: Int, scope: TScope): Unit = {
    if (returned.isDefined && returned.get != null && returned.get != expected && !returned.get.hasParent(expected)) {
      val msg =
        s"""type mismatch;
            found: ${returned.get.fullClassName}
            expected: ${expected.fullClassName}""".stripMargin

      TypeErrors.addError(scope, line, msg)
    }
  }

  private def checkValOrDefBody(scope: TScope, body: Tree, expectedType: TType): Option[TType] = {
    var expected = expectedType
    val returnType = checkBodyOptional(scope, body)
    if (expectedType == null && returnType.isDefined) expected = returnType.get // For type inference
    checkReturnExpected(returnType, expected, body.pos.line, scope)
    returnType
  }

  private def checkBody(scope: TScope, body: Tree): TType = {
    body match {
      case EmptyTree => EmptyType
      case _ => checkStatement(scope, body)
    }
    //    body match {
    //      case b: Block => Some(checkBlock(scope, b))
    //      case v: ValDef => Some(checkVal(scope, v))
    //      case d: DefDef => Some(checkDef(scope, d))
    //      case a: Assign => Some(checkAssign(scope, a))
    //      case a: Apply => Some(checkApply(scope, a))
    //      case s: Select => Some(checkSelect(scope, s))
    //      case i: Ident => Some(checkIdent(scope, i))
    //      case l: Literal => Some(checkLiteral(scope, l))
    //      case i: If => Some(checkIf(scope, i))
    //      case l: LabelDef => Some(checkLabel(scope, l))
    //      case f: Function => Some(checkFunction(scope, f))
    //      case EmptyTree => None
    //    }
  }

  private def checkBodyOptional(scope: TScope, body: Tree): Option[TType] = {
    checkBody(scope, body) match {
      case EmptyType => None
      case _@t => Option(t)
    }
  }

  private def checkBlock(scope: TScope, block: Block): TType = {
    //TODO new Scope?
    block.stats.foreach(checkStatement(scope, _))

    checkStatement(scope, block.expr)
  }

  private def checkStatement(scope: TScope, tree: Tree) = tree match {
    case b: Block => checkBlock(scope, b)
    case v: ValDef => checkVal(scope, v)
    case a: Assign => checkAssign(scope, a)
    case d: DefDef => checkDef(scope, d)
    case a: Apply => checkApply(scope, a)
    case s: Select => checkSelect(scope, s)
    case i: Ident => checkIdent(scope, i)
    case l: Literal => checkLiteral(scope, l)
    case i: If => checkIf(scope, i)
    case l: LabelDef => checkLabel(scope, l)
    case f: Function => checkFunction(scope, f)
  }

  private def checkApply(scope: TScope, apply: Apply): TType = {
    apply.fun match {
      case s@Select(New(a: AppliedTypeTree), name) =>
        val onType: TType = TypeUtils.findType(scope, a.tpt)
        val appliedTypes: List[TType] = a.args.map(TypeUtils.findType(scope, _))
        val line: Int = apply.pos.line
        val newType = TypeUtils.applyTypesOnType(scope, onType, appliedTypes, line)
        val args = checkArgs(scope, apply.args)
        TypeUtils.findConstructor(scope, line, args, newType).returnType
      case s@Select(n: New, name) =>
        val args = checkArgs(scope, apply.args)
        TypeUtils.applyConstructor(scope, args, n)
      case s@Select(qualifier, name) =>
        val args: Seq[TType] = checkArgs(scope, apply.args)
        val tpe: TType = checkQualifier(scope, qualifier)
        val selectName: String = name.toString
        val method: Method = TypeUtils.findMethod(scope, selectName, s.pos.line, args, tpe)
        if (method.generics.nonEmpty) {
          val appliedTypes = method.getAppliedTypes(args)
          method.applyTypes(appliedTypes).returnType
        } else method.returnType
      case i: Ident =>
        val args = checkArgs(scope, apply.args)
        checkIdent(scope, i, args)
    }
  }

  private def checkSelect(scope: TScope, select: Select): TType = {
    val tpe: TType = checkQualifier(scope, select.qualifier)
    val selectName: String = select.name.toString

    TypeUtils.findMethodOrFieldType(scope, selectName, select.pos.line, tpe)
  }

  private def checkQualifier(scope: TScope, qualifier: Tree): TType = qualifier match {
    case l: Literal => TypeUtils.findType(scope, l)
    case i: Ident => TypeUtils.findIdentifier(scope, i).tpe
    case a: Apply => checkApply(scope, a)
    case t: This => scope.findThis()
    case s: Select => checkSelect(scope, s)
  }

  private def checkArgs(scope: TScope, args: List[Tree]) = {
    args.map({
      case i: Ident => checkIdent(scope, i)
      case l: Literal => checkLiteral(scope, l)
      case b: Block => checkBlock(scope, b)
      case a: Apply => checkApply(scope, a)
      case s: Select => checkSelect(scope, s)
      case t: This => scope.findThis()
      case f: Function => checkFunction(scope, f)
    })
  }

  private def checkIdent(scope: TScope, ident: Ident, params: Seq[TType] = Vector()): TType = {
    val iName: String = ident.name.toString
    //TODO: check if field?
    scope.findMethod(iName, params) match {
      case Some(m) => m.returnType
      case None =>
        scope.findIdentifier(iName) match {
          case Some(i) => i.tpe
          case None =>
            val thisTpe: TType = scope.findThis()
            thisTpe.findField(thisTpe, iName) match {
              case Some(f) => f.tpe
              case None => TypeErrors.addError(scope, ident.pos.line, s"Value $iName not found")
            }
        }
    }
  }

  private def checkLiteral(scope: TScope, literal: Literal): TType = {
    TypeUtils.findType(scope, literal)
  }

  private def checkVal(scope: TScope, v: ValDef): TType = {
    val expected: TType = TypeUtils.findType(scope, v.tpt)
    val returnTpe: Option[TType] = scoped(scope, checkValOrDefBody(_: TScope, v.rhs, expected))
    returnTpe match {
      case Some(tpe) =>
        TypeUtils.createIdentifier(scope, v, tpe)
      case None => TypeErrors.addError(scope, v.pos.line, s"Value definition ${v.name} requires a valid return type")
    }

    TypeUtils.unitType(scope)
  }

  private def checkDef(scope: TScope, d: DefDef): TType = {
    val expected: TType = TypeUtils.findType(scope, d.tpt)

    val m: Method = TypeUtils.createMethod(scope, d)
    scoped(scope, (s: TScope) => {
      TypeUtils.addParamsToScope(s, m.params)
      checkValOrDefBody(s, d.rhs, expected)
    })
    scope.addMethod(m)

    TypeUtils.unitType(scope)
  }

  /**
   * Check while / do while
   * @param scope the current scope
   * @param l
   * @return
   */
  private def checkLabel(scope: TScope, l: LabelDef): TType = {
    l match {
      case LabelDef(n, _, t) =>
        scoped(scope, (s: TScope) => {
          s.addMethod(Method(Context(scope.currentFile, l.pos.line), n.toString, TypeUtils.unitType(scope), Vector()))
          t match {
            case i: If => checkIf(s, i) // while
            case b: Block => checkBlock(s, b) // do while
          }
        })
    }
  }

  private def checkIf(scope: TScope, i: If): TType = {
    checkCond(scope, i.cond)
    val thenTpe: Option[TType] = checkBodyOptional(scope, i.thenp)
    val elseTpe: Option[TType] = checkElse(scope, i.elsep)

    TypeUtils.findCommonBaseClass(scope, thenTpe, elseTpe)
  }

  private def checkCond(scope: TScope, t: Tree): Unit = {
    checkBodyOptional(scope, t).map(_ == baseTypes.boolean).
      getOrElse(TypeErrors.addError(scope, t.pos.line, "If condition must be a boolean expression"))
  }

  private def checkElse(scope: TScope, t: Tree): Option[TType] = {
    t match {
      case l: Literal => l.value.value match {
        case bu: BoxedUnit => None
        case _ => checkBodyOptional(scope, l)
      }
      case _ => checkBodyOptional(scope, t)
    }
  }

  private def checkAssign(scope: TScope, a: Assign): TType = {
    // TODO a.lhs might be select or apply
    val tpe = a.lhs match {
      case i: Ident => handleIdentAssign(scope, i)
      case _ => handleFieldAssign(scope, a.lhs)
    }

    scoped(scope, checkValOrDefBody(_: TScope, a.rhs, tpe))

    TypeUtils.unitType(scope)
  }

  private def handleIdentAssign(scope: TScope, i: Ident): TType = {
    val identifier = TypeUtils.findIdentifier(scope, i)
    if (!identifier.mutable) TypeErrors.addError(scope, i.pos.line, "Reassignment to val")
    identifier.tpe
  }

  private def handleFieldAssign(scope: TScope, t: Tree): TType = {
    t match {
      case s: Select => handleSelectFieldAssign(scope, s)
    }
  }

  private def handleSelectFieldAssign(scope: TScope, s: Select): TType = {
    def handleSelectFieldAssignAcc(subSelect: Select): Field = {
      subSelect.qualifier match {
        case t: This =>
          TypeUtils.findField(scope, subSelect.name.toString, t.pos.line, scope.findThis())
        case i: Ident =>
          val tpe: TType = checkIdent(scope, i)
          TypeUtils.findField(scope, subSelect.name.toString, i.pos.line, tpe)
        case subS: Select =>
          val f: Field = handleSelectFieldAssignAcc(subS)
          TypeUtils.findField(scope, subSelect.name.toString, subS.pos.line, f.tpe)
      }
    }
    val field = handleSelectFieldAssignAcc(s)
    checkFieldAssign(scope, s, field)
  }

  private def checkFieldAssign(scope: TScope, t: Tree, f: Field): TType = {
    if (!f.isMutable) TypeErrors.addError(scope, t.pos.line, "Reassignment to val")
    f.tpe
  }

  private def checkFunction(scope: TScope, f: Function) = {
    val params: Seq[Param] = TypeUtils.createParams(scope, f.vparams)
    val retType = scoped(scope, (s: TScope) => {
      TypeUtils.addParamsToScope(scope, params)
      checkBody(s, f.body)
    })
    TypeUtils.createFunctionTypeFromParams(scope, params, retType, f.pos.line)
  }

  private def scoped(scope: TScope, f: TScope => Option[TType]) = {
    val childScope: TScope = scope.enterScope(BlockScope)
    val tpe = f(childScope)
    childScope.exitScope()
    tpe
  }

  private def scoped(scope: TScope, f: TScope => TType) = {
    val childScope: TScope = scope.enterScope(BlockScope)
    val tpe = f(childScope)
    childScope.exitScope()
    tpe
  }

  private def scoped[U](scope: TScope, f: TScope => U) = {
    val childScope: TScope = scope.enterScope(BlockScope)
    f(childScope)
    childScope.exitScope()
  }

}
