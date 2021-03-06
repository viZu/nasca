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

  def checkTypes(rootScope: TSymbolTable, tree: AST): ScalaFileWrapper = {
    require(tree != null)
    profileFunc(logger, s"Type checker ${tree.fileName}", () => {
      val traverser: ClassTraverser = new ClassTraverser(rootScope, tree)
      traverser.traverse(tree.internalTree)
      traverser.createFileWrapper()
    }, Debug)
  }

  private class ClassTraverser(var currentScope: TSymbolTable, ast: AST) extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    val impls = new ArrayBuffer[Implementation]
    val imports = new ArrayBuffer[ImportStmt]
    var scoped: Boolean = false

    override def traverse(tree: Tree): Unit = {
      tree match {
        case c: ClassDef =>
          handleEnterChildScope()
          val thisTpe: TType = TypeUtils.findType(currentScope, c)
          checkImplementation(currentScope.enterScope(thisTpe), c)
          impls += getImplementation(c, thisTpe)
        case m: ModuleDef =>
          handleEnterChildScope()
          val thisTpe: TType = TypeUtils.findType(currentScope, m, searchObject = true)
          checkImplementation(currentScope.enterScope(thisTpe), m)
          impls += getImplementation(m, thisTpe)
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
        currentScope = currentScope.enterScope(FileScope)
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

  private def checkImplementation(scope: TSymbolTable, c: ImplDef) = {
    checkGenerics(scope)
    checkMembers(scope, c.impl)
  }

  private def checkGenerics(scope: TSymbolTable) = {
    scope.findThis() match {
      case gt: GenericType =>
        // TODO check generic for unique identifier
        gt.genericModifiers.foreach(scope.addTypeArgument)
      case _ =>
    }
  }

  private def checkMembers(scope: TSymbolTable, t: Template) = {
    t.body.foreach(m => checkMember(scope, m))
  }

  private def checkMember(scope: TSymbolTable, member: Tree): Unit = {
    member match {
      case v: ValDef => checkValMember(scope, v)
      case d: DefDef => checkDefMember(scope, d)
      case a: Apply => checkApply(scope, a)
      case s: Select => checkSelect(scope, s)
      case EmptyTree =>
      case _ => TypeErrors.addError(scope, member.pos.line, s"unrecognized member ${member.toString()}")
    }
  }

  private def checkValMember(scope: TSymbolTable, v: ValDef) = {
    val field: Field = TypeUtils.findField(scope, v)
    if (!field.isAbstract) {
      scope.scoped((s: TSymbolTable) => {
        checkValOrDefBody(s, v.rhs, field.tpe)
      }, BlockScope)
    }
  }

  private def checkDefMember(scope: TSymbolTable, d: DefDef) = {
    scope.scoped((s: TSymbolTable) => {
      TypeUtils.createAndAddGenericModifiers(s, d.tparams)
      val method: Method = TypeUtils.findMethodForDef(s, d)
      if (!method.constructor && !method.isAbstract) {
        val expected: TType = method.returnType
        TypeUtils.addParamsToScope(s, method.params)
        checkValOrDefBody(s, d.rhs, expected)
      }
    }, MethodScope)
  }

  private def checkReturnExpected(returned: Option[TType], expected: TType, line: Int, scope: TSymbolTable): Unit = {
    if (returned.isDefined && returned.get != null && returned.get != expected && !returned.get.hasParent(expected)) {
      val msg =
        s"""type mismatch;
            found: ${returned.get}
            expected: $expected""".stripMargin

      TypeErrors.addError(scope, line, msg)
    }
  }

  private def checkValOrDefBody(scope: TSymbolTable, body: Tree, expectedType: TType): Option[TType] = {
    var expected = expectedType
    val returnType = checkBodyOptional(scope, body)
    if (expectedType == null && returnType.isDefined) expected = returnType.get // For type inference
    checkReturnExpected(returnType, expected, body.pos.line, scope)
    returnType
  }

  private def checkBody(scope: TSymbolTable, body: Tree): TType = {
    body match {
      case EmptyTree => EmptyType
      case _ => checkStatement(scope, body)
    }
  }

  private def checkBodyOptional(scope: TSymbolTable, body: Tree): Option[TType] = {
    checkBody(scope, body) match {
      case EmptyType => None
      case _@t => Option(t)
    }
  }

  private def checkBlock(scope: TSymbolTable, block: Block): TType = {
    scope.scoped(childScope => {
      block.stats.foreach(checkStatement(scope, _))
      checkStatement(scope, block.expr)
    }, BlockScope)
  }

  private def checkStatement(scope: TSymbolTable, tree: Tree) = tree match {
    case b: Block => checkBlock(scope, b)
    case v: ValDef => checkVal(scope, v)
    case a: Assign => checkAssign(scope, a)
    case d: DefDef => checkDef(scope, d)
    case Apply(s: Super, lst: List[Tree]) => scope.findThis() // skip super call in constructor
    case a: Apply => checkApply(scope, a)
    case s: Select => checkSelect(scope, s)
    case i: Ident => checkIdent(scope, i)
    case l: Literal => checkLiteral(scope, l)
    case i: If => checkIf(scope, i)
    case l: LabelDef => checkLabel(scope, l)
    case f: Function => checkFunction(scope, f)
    //    case m: Match => checkMatch(scope, m)
    case t: This => scope.findThis()
  }

  private def checkApply(scope: TSymbolTable, apply: Apply): TType = {
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
        val tpe: TType = checkQualifier(scope, qualifier)
        val selectName: String = name.toString
        val args: Seq[TType] = checkArgs(scope, apply.args)
        val method: Method = TypeUtils.findMethod(scope, selectName, s.pos.line, args, tpe)
        if (method.generics.nonEmpty) {
          val appliedTypes = method.getAppliedTypes(args)
          method.applyTypes(scope, appliedTypes).returnType
        } else method.returnType
      case i: Ident =>
        val args = checkArgs(scope, apply.args)
        if (args.isEmpty) checkIdent(scope, i)
        else checkIdentApply(scope, i, args)
    }
  }

  private def checkSelect(scope: TSymbolTable, select: Select): TType = {
    val tpe: TType = checkQualifier(scope, select.qualifier)
    val selectName: String = select.name.toString

    TypeUtils.findMethodOrFieldType(scope, selectName, select.pos.line, tpe)
  }

  private def checkQualifier(scope: TSymbolTable, qualifier: Tree): TType = qualifier match {
    case l: Literal => TypeUtils.findType(scope, l)
    case i: Ident => TypeUtils.findIdent(scope, i.name.toString).tpe
    case a: Apply => checkApply(scope, a)
    case t: This => scope.findThis()
    case s: Select => checkSelect(scope, s)
  }

  private def checkArgs(scope: TSymbolTable, args: List[Tree]) = {
    args.map(checkStatement(scope, _))
  }

  private def checkIdent(scope: TSymbolTable, ident: Ident): TType = {
    val iName: String = ident.name.toString
    scope.findMethod(iName, Vector()) match {
      case Some(m) => m.returnType
      case None =>
        scope.findIdentifier(iName) match {
          case Some(i) => i.tpe
          case None =>
            val thisTpe: TType = scope.findThis()
            thisTpe.findField(thisTpe, iName) match {
              case Some(f) => f.tpe
              case None =>
                scope.findObject(iName) match {
                  case Some(o) => o
                  case None => TypeErrors.addError(scope, ident.pos.line, s"Value $iName not found")
                }
            }
        }
    }
  }

  private def checkIdentApply(scope: TSymbolTable, ident: Ident, params: Seq[TType] = Vector()): TType = {
    val iName: String = ident.name.toString
    scope.findMethod(iName, params) match {
      case Some(m) => m.returnType
      case None => TypeErrors.addError(scope, ident.pos.line, s"Value $iName not found")
    }
  }

  private def checkLiteral(scope: TSymbolTable, literal: Literal): TType = {
    TypeUtils.findType(scope, literal)
  }

  private def checkVal(scope: TSymbolTable, v: ValDef): TType = {
    val expected: TType = TypeUtils.findType(scope, v.tpt)
    val returnTpe: Option[TType] = scope.scoped(checkValOrDefBody(_: TSymbolTable, v.rhs, expected), BlockScope)
    returnTpe match {
      case Some(tpe) =>
        val typeToAdd = if (expected != null) expected else tpe
        TypeUtils.createIdentifier(scope, v, typeToAdd)
      case None => TypeErrors.addError(scope, v.pos.line, s"Value definition ${v.name} requires a valid return type")
    }
    TypeUtils.unitType(scope)
  }

  private def checkDef(scope: TSymbolTable, d: DefDef): TType = {
    val expected: TType = TypeUtils.findType(scope, d.tpt)
    val m: Method = TypeUtils.createMethod(scope, d)
    scope.addMethod(m)
    scope.scoped((s: TSymbolTable) => {
      TypeUtils.addParamsToScope(s, m.params)
      checkValOrDefBody(s, d.rhs, expected)
    }, MethodScope)
    TypeUtils.unitType(scope)
  }

  /**
    * Check while / do while
    *
    * @param scope the current scope
    * @param l
    * @return
    */
  private def checkLabel(scope: TSymbolTable, l: LabelDef): TType = l match {
    case LabelDef(n, _, t) =>
      scope.scoped((s: TSymbolTable) => {
        s.addMethod(Method(Context(scope.currentFile, l.pos.line), n.toString, TypeUtils.unitType(scope), Vector()))
        t match {
          case i: If => checkIf(s, i) // while
          case b: Block => checkBlock(s, b) // do while
        }
      }, BlockScope)
  }

  private def checkIf(scope: TSymbolTable, i: If): TType = {
    checkCond(scope, i.cond)
    val thenTpe: Option[TType] = checkBodyOptional(scope, i.thenp)
    val elseTpe: Option[TType] = checkElse(scope, i.elsep)

    TypeUtils.findCommonBaseClass(scope, thenTpe, elseTpe)
  }

  private def checkCond(scope: TSymbolTable, t: Tree): Unit = {
    checkBodyOptional(scope, t).map(_ == baseTypes.boolean).
      getOrElse(TypeErrors.addError(scope, t.pos.line, "If condition must be a boolean expression"))
  }

  private def checkElse(scope: TSymbolTable, t: Tree): Option[TType] = {
    t match {
      case l: Literal => l.value.value match {
        case bu: BoxedUnit => None
        case _ => checkBodyOptional(scope, l)
      }
      case _ => checkBodyOptional(scope, t)
    }
  }

  private def checkAssign(scope: TSymbolTable, a: Assign): TType = {
    // TODO a.lhs might be select or apply
    val tpe = a.lhs match {
      case i: Ident => handleIdentAssign(scope, i)
      case _ => handleFieldAssign(scope, a.lhs)
    }
    scope.scoped(checkValOrDefBody(_: TSymbolTable, a.rhs, tpe), BlockScope)
    TypeUtils.unitType(scope)
  }

  private def handleIdentAssign(scope: TSymbolTable, i: Ident): TType = {
    val identifier = TypeUtils.findIdentifier(scope, i)
    if (!identifier.mutable) TypeErrors.addError(scope, i.pos.line, "Reassignment to val")
    identifier.tpe
  }

  private def handleFieldAssign(scope: TSymbolTable, t: Tree): TType = {
    t match {
      case s: Select => handleSelectFieldAssign(scope, s)
    }
  }

  private def handleSelectFieldAssign(scope: TSymbolTable, s: Select): TType = {
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

  private def checkFieldAssign(scope: TSymbolTable, t: Tree, f: Field): TType = {
    if (!f.isMutable) TypeErrors.addError(scope, t.pos.line, "Reassignment to val")
    f.tpe
  }

  private def checkFunction(scope: TSymbolTable, f: Function) = {
    val params: Seq[Param] = TypeUtils.createParams(scope, f.vparams)
    val retType = scope.scoped((s: TSymbolTable) => {
      TypeUtils.addParamsToScope(s, params)
      checkBody(s, f.body)
    }, MethodScope)
    TypeUtils.createFunctionTypeFromParams(scope, params, retType, f.pos.line)
  }

  //  private def checkMatch(scope: TSymbolTable, m: Match): TType = {
  //    val selector = checkStatement(scope, m.selector)
  //    val checkedCases = m.cases.map(checkCase(scope, selector, _))
  //    TypeUtils.findCommonBaseClass(scope, checkedCases)
  //  }
  //
  //  private def checkCase(scope: TSymbolTable, selectorType: TType, c: CaseDef) = {
  //    scope.scoped((s: TSymbolTable) => {
  //      checkPattern(s, selectorType, c.pat)
  //      checkStatement(s, c.body)
  //    }, BlockScope)
  //  }
  //
  //  private def checkPattern(scope: TSymbolTable, selectorType: TType, pat: Tree) = {
  //    pat match {
  //      case l: Literal => checkLiteral(scope, l)
  //      case ident: Ident => TypeUtils.findIdent(scope, ident.name.toString) match {
  //        case i: Identifier if i.mutable => throw new RuntimeException
  //        case m: Method => throw new RuntimeException
  //        case f: Field if f.isMutable => throw new RuntimeException
  //        case _@s => s.tpe
  //      }
  //      case a: Apply => TypeUtils.unitType(scope)
  //      case b: Bind => TypeUtils.unitType(scope)
  //    }
  //  }
}
