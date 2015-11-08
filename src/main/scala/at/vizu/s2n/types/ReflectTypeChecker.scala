package at.vizu.s2n.types

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.result.{ClassImplementation, Implementation, ObjectImplementation, ScalaFileWrapper}
import at.vizu.s2n.types.symbol._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 23.10.15.
 */
class ReflectTypeChecker(baseTypes: BaseTypes) extends TypeChecker {

  def checkTypes(rootScope: TScope, tree: AST): ScalaFileWrapper = {
    require(tree != null)
    println(s"Checking types for file ${tree.fileName}")
    val traverser: ClassTraverser = new ClassTraverser(rootScope, tree)
    traverser.traverse(tree.internalTree)
    traverser.createFileWrapper()
  }

  private class ClassTraverser(var currentScope: TScope, ast: AST) extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    val impls = new ArrayBuffer[Implementation]
    val imports = new ArrayBuffer[String]
    var scoped: Boolean = false

    override def traverse(tree: Tree): Unit = {
      tree match {
        case c: ImplDef =>
          val thisTpe: TType = TypeUtils.findType(currentScope, c)
          checkImplementation(currentScope.enterScope(thisTpe), c)
          impls += getImplementation(c)
        case PackageDef(Ident(name), subtree) =>
          pkgBuilder.append(name.toString)
          super.traverse(tree)
        case i: Import =>
          if (!scoped) {
            currentScope = currentScope.enterScope()
            currentScope.currentFile = ast.fileName
            currentScope.currentPackage = packageName
            scoped = true
          }
          handleImport(i)
          imports += i.toString()
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
        .getOrElse(throw new TypeException(currentScope.currentFile, line, s"No type with name $typeName found"))
      currentScope.addTypeAlias(selector.rename.toString, typeName)
    }

    private def getImplementation(implDef: ImplDef) = {
      implDef match {
        case c: ClassDef => ClassImplementation(c)
        case m: ModuleDef => ObjectImplementation(m)
      }
    }

    def createFileWrapper() = {
      ScalaFileWrapper(impls, imports, packageName)
    }
  }

  private def checkImplementation(scope: TScope, c: ImplDef) = {
    checkMembers(scope, c.impl)
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
      case _ => throw new TypeException(scope.currentFile, member.pos.line, s"unrecognized member ${member.toString()}")
    }
  }

  private def checkValMember(scope: TScope, v: ValDef) = {
    val field: Field = TypeUtils.findField(scope, v)
    if (!field.isAbstract) {
      scoped(scope, (s: TScope) => {
        val returnType: Option[TType] = checkValOrDefBody(s, v.rhs, field.tpe)
        returnType match {
          case Some(tpe) => if (field.tpe == null) field.tpe = tpe
          case None => if (field.tpe == null) throw new TypeException(scope.currentFile, v.pos.line,
            s"Value definition ${v.name} requires a valid return type")
        }
      })
    }
  }

  private def checkDefMember(scope: TScope, d: DefDef) = {
    val method: Method = TypeUtils.findMethodForDef(scope, d)
    if (!method.constructor && !method.isAbstract) {
      val expected: TType = TypeUtils.findType(scope, d.tpt)
      scoped(scope, (s: TScope) => {
        addParamsToScope(s, method.params)
        checkValOrDefBody(s, d.rhs, expected)
      })
    }
  }

  private def checkReturnExpected(returned: Option[TType], expected: TType, line: Int, scope: TScope): Unit = {
    if (returned.isDefined && returned.get != null && returned.get != expected && !returned.get.hasParent(expected)) {
      val msg =
        s"""type mismatch;
            found: ${returned.get.fullClassName}
            expected: ${expected.fullClassName}""".stripMargin

      throw new TypeException(scope.currentFile, line, msg)
    }
  }

  private def addParamsToScope(scope: TScope, params: Seq[Param]) = {
    params.foreach(p => scope.add(Identifier(p.ctx, p.name, p.tpe, p.mutable)))
  }

  private def checkValOrDefBody(scope: TScope, body: Tree, expectedType: TType): Option[TType] = {
    var expected = expectedType
    val returnType = checkBody(scope, body)
    if (expectedType == null && returnType.isDefined) expected = returnType.get // For type inference
    checkReturnExpected(returnType, expected, body.pos.line, scope)
    returnType
  }

  private def checkBody(scope: TScope, body: Tree): Option[TType] = {
    body match {
      case b: Block => Some(checkBlock(scope, b))
      case a: Apply => Some(checkApply(scope, a))
      case s: Select => Some(checkSelect(scope, s))
      case i: Ident => Some(checkIdent(scope, i))
      case l: Literal => Some(checkLiteral(scope, l))
      case i: If => Some(checkIf(scope, i))
      case l: LabelDef => Some(checkLabel(scope, l))
      case f: Function => throw new TypeException(scope.currentFile, body.pos.line, "Anonymous functions are currently not supported")
      case EmptyTree => None
    }
  }

  private def checkBlock(scope: TScope, block: Block): TType = {
    //TODO new Scope?
    println(block)
    block.stats.foreach {
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
      case f: Function => throw new TypeException(scope.currentFile, block.pos.line,
        "Anonymous functions are currently not supported")
    }

    block.expr match {
      case b: Block => checkBlock(scope, b)
      case v: ValDef => checkVal(scope, v)
      case d: DefDef => checkDef(scope, d)
      case a: Apply => checkApply(scope, a)
      case s: Select => checkSelect(scope, s)
      case i: Ident => checkIdent(scope, i)
      case l: Literal => checkLiteral(scope, l)
      case i: If => checkIf(scope, i)
      case l: LabelDef => checkLabel(scope, l)
      case f: Function => throw new TypeException(scope.currentFile, block.pos.line,
        "Anonymous functions are currently not supported")
    }
  }

  private def checkApply(scope: TScope, apply: Apply): TType = {
    apply.fun match {
      case s: Select =>
        val tpe: TType = checkQualifier(scope, s.qualifier)
        val args: Seq[TType] = checkArgs(scope, apply.args)
        val selectName: String = s.name.toString

        TypeUtils.findMethod(scope, selectName, s.pos.line, args, tpe).returnType
      case i: Ident =>
        checkIdent(scope, i)
    }
  }

  private def checkSelect(scope: TScope, select: Select): TType = {
    val tpe: TType = checkQualifier(scope, select.qualifier)
    val selectName: String = select.name.toString

    TypeUtils.findMethodOrFieldType(scope, selectName, select.pos.line, tpe)
  }

  private def checkQualifier(scope: TScope, qualifier: Tree): TType = qualifier match {
    case l: Literal => TypeUtils.findType(scope, l)
    case n: New => TypeUtils.findType(scope, n)
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
    })
  }

  private def checkIdent(scope: TScope, ident: Ident): TType = {
    val iName: String = ident.name.toString
    //TODO: check if field?
    scope.findMethod(iName, Seq()) match {
      case Some(m) => m.returnType
      case None =>
        scope.findIdentifier(iName) match {
          case Some(i) => i.tpe
          case None =>
            scope.findThis().findField(iName) match {
              case Some(f) => f.tpe
              case None => throw new TypeException(scope.currentFile, ident.pos.line, s"Value $iName not found")
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
      case None => throw new TypeException(scope.currentFile, v.pos.line,
        s"Value definition ${v.name} requires a valid return type")
    }

    TypeUtils.unitType(scope)
  }

  private def checkDef(scope: TScope, d: DefDef): TType = {
    val expected: TType = TypeUtils.findType(scope, d.tpt)

    val m: Method = TypeUtils.createMethod(scope, d)
    scoped(scope, (s: TScope) => {
      addParamsToScope(s, m.params)
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
          s.addMethod(Method(Context(scope.currentFile, l.pos.line), n.toString, TypeUtils.unitType(scope), Seq()))
          t match {
            case i: If => checkIf(s, i) // while
            case b: Block => checkBlock(s, b) // do while
          }
        })
    }
  }

  private def checkIf(scope: TScope, i: If): TType = {
    checkCond(scope, i.cond)
    val thenTpe: Option[TType] = checkBody(scope, i.thenp)
    val elseTpe: Option[TType] = checkElse(scope, i.elsep)

    TypeUtils.findCommonBaseClass(scope, thenTpe, elseTpe)
  }

  private def checkCond(scope: TScope, t: Tree): Unit = {
    checkBody(scope, t).map(_ == baseTypes.boolean).
      orElse(throw new TypeException(scope.currentFile, t.pos.line, "If condition must be a boolean expression"))
  }

  private def checkElse(scope: TScope, t: Tree): Option[TType] = {
    t match {
      case l: Literal => l.value.value match {
        case bu: BoxedUnit => None
        case _ => checkBody(scope, l)
      }
      case _ => checkBody(scope, t)
    }
  }

  private def checkAssign(scope: TScope, a: Assign): TType = {
    val identifier = TypeUtils.findIdentifier(scope, a.lhs.asInstanceOf[Ident])
    if (!identifier.mutable) throw new TypeException(scope.currentFile, a.pos.line, "Reassignment to val")
    scoped(scope, checkValOrDefBody(_: TScope, a.rhs, identifier.tpe))

    TypeUtils.unitType(scope)
  }

  private def scoped(scope: TScope, f: TScope => Option[TType]) = {
    val childScope: TScope = scope.enterScope()
    val tpe = f(childScope)
    childScope.exitScope()
    tpe
  }

  private def scoped(scope: TScope, f: TScope => TType) = {
    val childScope: TScope = scope.enterScope()
    val tpe = f(childScope)
    childScope.exitScope()
    tpe
  }

  private def scoped(scope: TScope, f: TScope => Unit) = {
    val childScope: TScope = scope.enterScope()
    f(childScope)
    childScope.exitScope()
  }

}
