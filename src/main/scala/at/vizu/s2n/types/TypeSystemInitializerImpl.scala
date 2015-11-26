package at.vizu.s2n.types

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
class TypeSystemInitializerImpl(scopeInitializer: ScopeInitializer) extends TypeSystemInitializer {

  private val rootScope: TScope = scopeInitializer.initScope
  private var currentScope = rootScope
  private var trees: Seq[AST] = Seq()

  override def initTypeSystem(trees: Seq[AST]): TScope = {
    println("Initializing type system")
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
    println("Gathering types")
    TypeGatherer.gatherTypes(trees, currentScope)
  }

  /**
   * Init Scope Phase 2
   */

  private def initScopePhase2() = {
    trees.foreach(tree => {
      currentScope.currentFile = tree.fileName
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
    var tpe: TType = null
    var scoped: Boolean = false

    override def traverse(tree: Tree): Unit = {
      //ln(tree.toString())
      tree match {
        case c: ClassDef =>
          handleEnterChildScope()
          tpe = enhanceClass(packageName, c)
        case m: ModuleDef =>
          handleEnterChildScope()
          tpe = enhanceObject(packageName, m)
        case PackageDef(Ident(name), subtree) =>
          pkgBuilder.append(name.toString)
          super.traverse(tree)
        case i: Import =>
          handleEnterChildScope()
          handleImport(i)
        case _ => super.traverse(tree)
      }
    }

    def packageName = {
      pkgBuilder.mkString(".")
    }

    private def handleEnterChildScope() = {
      if (!scoped) {
        currentScope = currentScope.enterScope()
        currentScope.currentPackage = packageName
        scoped = true
      }
    }
  }

  private def handleImport(i: Import) = {
    i.selectors.foreach(s => addTypeAlias(i.expr.toString(), s, i.pos.line))
  }

  private def addTypeAlias(pkgName: String, selector: ImportSelector, line: Int): Unit = {
    val typeName: String = pkgName + "." + selector.name.toString
    currentScope.findClass(typeName)
      .orElse(throw new TypeException(currentScope.currentFile, line, s"No type with name $typeName found")).get
    currentScope.addTypeAlias(selector.rename.toString, typeName)
  }

  /**
   * Enhance Class
   */

  private def enhanceClass(pkgName: String, c: ClassDef): TType = {
    enhanceType(pkgName, c, rootScope.findClass)
  }

  /**
   * Create Module
   */

  private def enhanceObject(pkgName: String, m: ModuleDef): TType = {
    enhanceType(pkgName, m, rootScope.findObject)
  }

  private def enhanceType(pkgName: String, m: ImplDef, typeProvider: String => Option[TType]): TType = {
    val className = if (pkgName.isEmpty) m.name.toString else pkgName + "." + m.name.toString
    val tpe = typeProvider(className)
      .orElse(throw new scala.RuntimeException(s"No type with name $className found")).get

    val traverser = new ClassMemberTraverser(m.impl)
    traverser.buildMembers()
    traverser.fields.foreach(tpe.addField)
    traverser.methods.foreach(tpe.addMethod)
    traverser.parents.foreach(tpe.addParent)
    tpe
  }

  private class ClassMemberTraverser(t: Template) {

    val methods = new ArrayBuffer[Method]
    val fields = new ArrayBuffer[Field]
    val parents = new ArrayBuffer[TType]

    def buildMembers() = {
      // Todo self type
      parents ++= t.parents.map {
        case s@Select(i, tn) => TypeUtils.findType(currentScope, s)
        case i: Ident => TypeUtils.findType(currentScope, i)
      }
      t.body.foreach {
        case d: DefDef => methods += createMethod(d)
        case v: ValDef => fields += createField(v)
        case _@rest => println("TemplateTraverser: " + rest.getClass.getName)
      }
    }
  }

  private def createMethod(d: DefDef): Method = {
    TypeUtils.createMethod(currentScope, d)
  }

  private def createField(v: ValDef): Field = {
    val ctx = Context(currentScope.currentFile, v.pos.line)
    Field(ctx, TypeUtils.getModifiers(v.mods), v.name.toString, TypeUtils.findType(currentScope, v.tpt))
  }

  /**
   * Validate Types
   */
  private def validateTypes() = {
    println("Validating gathered types")
    rootScope.types.foreach(_.validate())
    rootScope.objects.foreach(_.validate())
  }
}