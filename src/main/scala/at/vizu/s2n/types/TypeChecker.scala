package at.vizu.s2n.types

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 23.10.15.
 */
class TypeChecker(rootScope: TScope) {

  lazy val unitType = rootScope.findClass("scala.Unit").get
  lazy val booleanType = rootScope.findClass("scala.Boolean").get
  var currentScope = rootScope

  def checkTypes(tree: AST) = {
    require(tree != null)
    println(s"Checking types for file ${tree.fileName}")
    val traverser: ClassTraverser = new ClassTraverser(tree)
    traverser.traverse(tree.internalTree)
  }

  private class ClassTraverser(ast: AST) extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    var scoped: Boolean = false

    override def traverse(tree: Tree): Unit = {
      //ln(tree.toString())
      tree match {
        case c: ImplDef =>
          val thisTpe: TType = TypeUtils.findType(currentScope, c)
          checkImplementation(currentScope.enterScope(thisTpe), c)
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
        case _ => super.traverse(tree)
      }
    }

    def packageName = {
      pkgBuilder.mkString(".")
    }

    private def handleImport(i: Import) = {
      i.selectors.filter(s => s.name.toString != s.rename.toString)
        .foreach(s => addTypeAlias(i.expr.toString(), s, i.pos.line))
    }

    private def addTypeAlias(pkgName: String, selector: ImportSelector, line: Int): Unit = {
      val typeName: String = pkgName + "." + selector.name.toString
      currentScope.findClass(typeName)
        .getOrElse(throw new TypeException(currentScope.currentFile, line, s"No type with name $typeName found"))
      currentScope.addTypeAlias(selector.rename.toString, typeName)
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
      case _ => throw new TypeException(scope.currentFile, member.pos.line, s"unrecognized member ${member.toString()}")
    }
  }

  private def checkValMember(scope: TScope, v: ValDef) = {
    val field: Field = TypeUtils.findField(scope, v)
    if (!field.isAbstract) {
      scoped(scope, (s: TScope) => checkValOrDefBody(s, v.rhs, field.tpe))
    }
  }

  private def checkDefMember(scope: TScope, d: DefDef) = {
    val method: Method = TypeUtils.findMethodForDef(scope, d)
    if (!method.constructor) {
      // TODO handle constructor
      if (!method.isAbstract) {
        val expected: TType = TypeUtils.findType(scope, d.tpt)
        scoped(scope, (s: TScope) => checkValOrDefBody(s, d.rhs, expected))
      }
    }
  }

  private def checkReturnExpected(returned: Option[TType], expected: TType, line: Int, scope: TScope): Unit = {
    if (returned.isDefined && returned.get != expected && !returned.get.hasParent(expected)) {
      throw new TypeException(scope.currentFile, line,
        s"type ${returned.get.fullClassName} did not match expected type ${expected.fullClassName}")
    }
  }

  private def addParamsToScope(scope: TScope, params: Seq[Param]) = {
    val childScope = scope.enterScope()
    params.foreach(p => childScope.add(Identifier(p.ctx, p.name, p.tpe, p.mutable)))
    childScope
  }

  private def checkValOrDefBody(scope: TScope, body: Tree, expectedType: TType): Unit = {
    val returnType = checkBody(scope, body)
    checkReturnExpected(returnType, expectedType, body.pos.line, scope)
  }

  private def checkBody(scope: TScope, body: Tree): Option[TType] = {
    body match {
      case b: Block => Some(checkBlock(scope, b))
      case a: Apply => Some(checkApply(scope, a))
      case i: Ident => Some(checkIdent(scope, i))
      case l: Literal => Some(checkLiteral(scope, l))
      case i: If => Some(checkIf(scope, i))
      case l: LabelDef => Some(checkLabel(scope, l))
      case f: Function => throw new TypeException(currentScope.currentFile, body.pos.line, "Anonymous functions are currently not supported")
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
      case i: Ident => checkIdent(scope, i)
      case l: Literal => checkLiteral(scope, l)
      case i: If => checkIf(scope, i)
      case l: LabelDef => checkLabel(scope, l)
      case f: Function => throw new TypeException(currentScope.currentFile, block.pos.line,
        "Anonymous functions are currently not supported")
    }

    block.expr match {
      case b: Block => checkBlock(scope, b)
      case v: ValDef => checkVal(scope, v)
      case d: DefDef => checkDef(scope, d)
      case a: Apply => checkApply(scope, a)
      case i: Ident => checkIdent(scope, i)
      case l: Literal => checkLiteral(scope, l)
      case i: If => checkIf(scope, i)
      case l: LabelDef => checkLabel(scope, l)
      case f: Function => throw new TypeException(currentScope.currentFile, block.pos.line,
        "Anonymous functions are currently not supported")
    }
  }

  private def checkApply(scope: TScope, apply: Apply): TType = {
    apply.fun match {
      case s: Select =>
        val tpe: TType = checkQualifier(scope, s.qualifier)
        val args: Seq[TType] = checkArgs(scope, apply.args)
        val methodName: String = s.name.toString

        TypeUtils.findMethod(scope, methodName, s.pos.line, args, tpe).returnType
      case i: Ident =>
        checkIdent(scope, i)
    }
  }

  private def checkQualifier(scope: TScope, qualifier: Tree): TType = qualifier match {
    case l: Literal => TypeUtils.findType(scope, l)
    case i: Ident => TypeUtils.findIdentifier(scope, i).tpe
    case a: Apply => checkApply(scope, a)
  }

  private def checkArgs(scope: TScope, args: List[Tree]) = {
    args.map({
      case i: Ident => checkIdent(scope, i)
      case l: Literal => checkLiteral(scope, l)
      case b: Block => checkBlock(scope, b)
      case a: Apply => checkApply(scope, a)
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
          case None => throw new TypeException(scope.currentFile, ident.pos.line, s"value $iName not found")
        }
    }
  }

  private def checkLiteral(scope: TScope, literal: Literal): TType = {
    TypeUtils.findType(scope, literal)
  }

  private def checkVal(scope: TScope, v: ValDef): TType = {
    val expected: TType = TypeUtils.findType(scope, v.tpt)
    val childScope = scope.enterScope()
    checkValOrDefBody(childScope, v.rhs, expected)
    childScope.exitScope()

    TypeUtils.createIdentifier(scope, v)

    unitType
  }

  private def checkDef(scope: TScope, d: DefDef): TType = {
    val expected: TType = TypeUtils.findType(scope, d.tpt)
    val childScope = scope.enterScope()
    checkValOrDefBody(childScope, d.rhs, expected)
    childScope.exitScope()

    val m: Method = TypeUtils.createMethod(scope, d)
    scope.addMethod(m)

    unitType
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
        val childScope: TScope = scope.enterScope()
        childScope.addMethod(Method(Context(scope.currentFile, l.pos.line), n.toString, unitType, Seq()))
        val tpe = t match {
          case i: If => checkIf(childScope, i) // while
          case b: Block => checkBlock(childScope, b) // do while
        }
        childScope.exitScope()
        tpe
    }
  }

  private def checkIf(scope: TScope, i: If): TType = {
    checkCond(scope, i.cond)
    val thenTpe: Option[TType] = checkBody(scope, i.thenp)
    val elseTpe: Option[TType] = checkElse(scope, i.elsep)

    TypeUtils.findCommonBaseClass(scope, thenTpe, elseTpe)
  }

  private def checkCond(scope: TScope, t: Tree): Unit = {
    checkBody(scope, t).map(_ == booleanType).
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
    scoped(scope, checkValOrDefBody(_, a.rhs, identifier.tpe))
    unitType
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
