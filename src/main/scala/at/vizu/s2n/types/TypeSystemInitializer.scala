package at.vizu.s2n.types

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.util.TypeUtils
import at.vizu.s2n.types.symbol.{Context, Field, Method, Param, Scope, ScopeInitializer, Type, TypeGatherer}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Phil on 23.10.15.
 */
class TypeSystemInitializer {
  self: ScopeInitializer =>

  private val rootScope: Scope = initScope
  private var currentScope = rootScope
  private var currentFile = ""
  private var trees: Seq[AST] = Seq()

  def initTypeSystem(trees: Seq[AST]): Scope = {
    this.trees = trees
    initScopePhase1()
    initScopePhase2()
    validateTypes()
    rootScope
  }

  /**
   * Init Scope Phase 1
   */

  private def initScopePhase1() = {
    TypeGatherer.gatherTypes(trees, currentScope)
  }

  /**
   * Init Scope Phase 2
   */

  private def initScopePhase2() = {
    trees.foreach(tree => {
      currentFile = tree.fileName
      initTree(tree)
    })
  }

  private def initTree(tree: AST) = {
    val cTraverser: ClassTraverser = new ClassTraverser
    cTraverser.traverse(tree.internalTree)
    currentScope = if (cTraverser.scoped) currentScope.exitScope() else currentScope
  }

  private class ClassTraverser extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    var tpe: Type = null
    var scoped: Boolean = false

    override def traverse(tree: Tree): Unit = {
      //ln(tree.toString())
      tree match {
        case c: ClassDef =>
          tpe = enhanceClass(packageName, c)
        case m: ModuleDef =>
          tpe = enhanceObject(packageName, m)
        case PackageDef(Ident(name), subtree) =>
          println(showRaw(subtree))
          pkgBuilder.append(name.decoded)
          super.traverse(tree)
        case i: Import =>
          if (!scoped) {
            currentScope = currentScope.enterScope()
            scoped = true
          }
          handleImport(i)
        case _ => super.traverse(tree)
      }
    }

    def packageName = {
      pkgBuilder.mkString(".")
    }
  }

  private def handleImport(i: Import) = {
    i.selectors.filter(s => s.name.toString != s.rename.toString)
      .foreach(s => addTypeAlias(i.expr.toString(), s, i.pos.line))
  }

  private def addTypeAlias(pkgName: String, selector: ImportSelector, line: Int): Unit = {
    val typeName: String = pkgName + "." + selector.name.toString
    currentScope.findClass(typeName)
      .orElse(throw new TypeException(currentFile, line, s"No type with name $typeName found")).get
    currentScope.addTypeAlias(selector.rename.toString, typeName)
  }

  /**
   * Enhance Class
   */

  private def enhanceClass(pkgName: String, c: ClassDef): Type = {
    enhanceType(pkgName, c, rootScope.findClass)
  }

  /**
   * Create Module
   */

  private def enhanceObject(pkgName: String, m: ModuleDef): Type = {
    enhanceType(pkgName, m, rootScope.findObject)
  }

  private def enhanceType(pkgName: String, m: ImplDef, typeProvider: String => Option[Type]): Type = {
    val className = if (pkgName.isEmpty) m.name.toString else pkgName + "." + m.name.toString
    val tpe = typeProvider(className)
      .orElse(throw new scala.RuntimeException(s"No type with name $className found")).get

    val traverser = new ClassMemberTraverser
    traverser.traverse(m)
    traverser.fields.foreach(tpe.addField)
    traverser.methods.foreach(tpe.addMethod)
    traverser.parents.foreach(tpe.addParent)
    tpe
  }

  private class ClassMemberTraverser extends Traverser {

    val methods = new ArrayBuffer[Method]
    val fields = new ArrayBuffer[Field]
    val parents = new ArrayBuffer[Type]

    override def traverse(tree: Tree): Unit = {
      tree match {
        case t: Template => buildMembers(t)
        case _@rest =>
          println("ClassMemberTraverser: " + rest.getClass.getName)
          super.traverse(rest)
      }
    }

    private def buildMembers(t: Template) = {
      // Todo self type
      parents ++= t.parents.map {
        case s@Select(i, tn) => findType(s)
        case i: Ident => findType(i)
      }
      t.body.foreach {
        case d: DefDef => methods += createMethod(d)
        case v: ValDef =>
          if (!v.toString().endsWith("_")) {
            // self type?
            fields += createField(v)
          }
        case _@rest => println("TemplateTraverser: " + rest.getClass.getName)
      }
    }
  }

  private def createMethod(d: DefDef): Method = {
    def isConstructor(d: DefDef) = {
      d.name.toString == "<init>"
    }
    val params: List[Param] = d.vparamss.head.map {
      case v: ValDef => Param(findType(v.tpt), v.name.toString, v.rhs != EmptyTree)
    }
    val ctx = Context(currentFile, d.pos.line)
    val retType = findType(d.tpt)
    Method(ctx, d.name.toString, retType, TypeUtils.getModifiers(d.mods), params, isConstructor(d))
  }

  private def createField(v: ValDef): Field = {
    val ctx = Context(currentFile, v.pos.line)
    Field(ctx, TypeUtils.getModifiers(v.mods), v.name.toString, findType(v.tpt))
  }

  private def findType(typeTree: Tree): Type = {
    def throwTypeNotFound(typeName: String): Nothing = {
      val line = typeTree.pos.line
      val msg = s"value $typeName not found"
      throw new TypeException(currentFile, typeTree.pos.line, msg)
    }
    typeTree match {
      case i: Ident => currentScope.findClass(i.name.toString).getOrElse(throwTypeNotFound(i.name.toString))
      case s: Select => currentScope.findClass(s.toString()).getOrElse(throwTypeNotFound(s.toString()))
      case att: AppliedTypeTree => throw new RuntimeException(s"Generic Types not supported yet: $att")
      case tt: TypeTree => null
      case _@other => throw new RuntimeException(s"Unknown Typetree: ${showRaw(other)}")
    }
  }

  /**
   * Validate Types
   */
  def validateTypes() = {
    rootScope.types.foreach(_.validate())
    rootScope.objects.foreach(_.validate())
  }

}
