package at.vizu.s2n.types

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.generator.expression.{ExpressionOptions, Expression}
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
  private var trees: Seq[AST] = Vector()

  override def initTypeSystem(trees: Seq[AST]): TScope = {
    println("Initializing type system")
    this.trees = trees
    initScopePhase1()
    initScopePhase2()
    initScopePhase3()
    initScopePhase4()
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
    println("Applying generic modifiers")
    trees.foreach(t => {
      currentScope.currentFile = t.fileName
      val traverser: Phase2Traverser = new Phase2Traverser
      traverser.traverse(t.internalTree)
    })
  }

  private class Phase2Traverser extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    var scoped = false

    override def traverse(tree: Tree): Unit = tree match {
      case c: ClassDef =>
        val tpe: ConcreteType = getType(packageName, c, rootScope.findClass)
        handleEnterChildScope()
        addGenericModifiers(c.tparams, tpe)
      case PackageDef(Ident(name), subtree) =>
        pkgBuilder.append(name.toString)
        super.traverse(tree)
      case _ => super.traverse(tree)
    }

    private def handleEnterChildScope() = {
      if (!scoped) {
        currentScope = currentScope.enterScope()
        currentScope.currentPackage = packageName
        scoped = true
      }
    }

    private def packageName = pkgBuilder.mkString(".")
  }

  private def addGenericModifiers(generics: Seq[TypeDef], tpe: ConcreteType) = {
    tpe match {
      case g: GenericType =>
        generics.map(TypeUtils.createGenericModifier(currentScope, _)).foreach(gm => {
          currentScope.addClass(gm)
          g.addGenericModifier(gm)
        })
      case _ =>
    }
  }

  /**
    * Init Scope Phase 3
    */

  private def initScopePhase3() = {
    println("Gathering constructors")
    trees.foreach(t => {
      val traverser = new Phase3Traverser
      traverser.traverse(t.internalTree)
    })
  }

  private class Phase3Traverser extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]
    var scoped = false

    override def traverse(tree: Tree): Unit = tree match {
      case c: ClassDef =>
        val tpe: ConcreteType = getType(packageName, c, rootScope.findClass)
        handleEnterChildScope()
        addConstructorToTpe(currentScope, c.impl.body, tpe)
      case m: ModuleDef =>
        val tpe: ConcreteType = getType(packageName, m, rootScope.findObject)
        handleEnterChildScope()
        addConstructorToTpe(currentScope, m.impl.body, tpe)
      case PackageDef(Ident(name), subtree) =>
        pkgBuilder.append(name.toString)
        super.traverse(tree)
      case _ => super.traverse(tree)
    }

    private def handleEnterChildScope() = {
      if (!scoped) {
        currentScope = currentScope.enterScope()
        currentScope.currentPackage = packageName
        scoped = true
      }
    }

    private def packageName = pkgBuilder.mkString(".")
  }

  private def addConstructorToTpe(scope: TScope, member: List[Tree], tpe: ConcreteType) = {
    member.collect({ case d: DefDef => d }).filter(d => TypeUtils.isConstructor(d.name.toString)).foreach(d => {
      val constructor = TypeUtils.createMethod(scope, d)
      tpe.addMethod(constructor)
    })
  }

  /**
    * Init Scope Phase 4
   */

  private def initScopePhase4() = {
    println("Filling Types")
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
          val thisTpe: TType = TypeUtils.findType(currentScope, c)
          currentScope = currentScope.enterScope(thisTpe)
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
    println("Enhance Class " + c.name)
    val tpe: ConcreteType = getType(pkgName, c, rootScope.findClass)
    enhanceType(pkgName, c.impl, tpe)
  }

  /**
    * Create Module
    */

  private def enhanceObject(pkgName: String, m: ModuleDef): TType = {
    val tpe: ConcreteType = getType(pkgName, m, rootScope.findObject)
    enhanceType(pkgName, m.impl, tpe)
  }

  private def getType(pkgName: String, i: ImplDef, typeProvider: String => Option[TType]) = {
    val className = if (pkgName.isEmpty) i.name.toString else pkgName + "." + i.name.toString
    typeProvider(className).collect({ case c: ConcreteType => c })
      .orElse(throw new scala.RuntimeException(s"No type with name $className found")).get
  }

  private def enhanceType(pkgName: String, template: Template, tpe: ConcreteType): TType = {
    val traverser = new ClassMemberTraverser(template, tpe)
    traverser.buildMembers()
    traverser.parents.foreach(tpe.addParent)
    traverser.fields.foreach(tpe.addField)
    traverser.methods.foreach(tpe.addMethod)
    tpe
  }

  private class ClassMemberTraverser(t: Template, concreteType: ConcreteType) {

    val methods = new ArrayBuffer[Method]
    val fields = new ArrayBuffer[Field]
    val parents = new ArrayBuffer[Parent]

    def buildMembers() = {
      println("Build Member: " + concreteType)
      t.body.foreach {
        case d: DefDef => if (!TypeUtils.isConstructor(d.name.toString)) methods += createMethod(d)
        case v: ValDef => fields += createField(v)
        case _@rest => println("TemplateTraverser: " + rest.getClass.getName)
      }
      // Todo self type
      println("Build Parents: " + concreteType)
      parents ++= t.parents.map {
        case s@Select(i, tn) => Parent(TypeUtils.findType(currentScope, s))
        case a: AppliedTypeTree =>
          val tpe = TypeUtils.findType(currentScope, a)
          Parent(tpe)
        case i: Ident => Parent(TypeUtils.findType(currentScope, i))
        case Apply(t: Tree, p: List[Tree]) =>
          val tpe: TType = TypeUtils.findType(currentScope, t)
          val bt: BaseTypes = currentScope.baseTypes
          val args = TypeInference.getTypes(bt, currentScope, p)
          TypeUtils.findConstructor(currentScope, t.pos.line, args, tpe)
          val expressions = p.map(Expression(bt, currentScope, _, ExpressionOptions(true)))
          Parent(tpe, expressions)
      }
    }
  }

  private def createMethod(d: DefDef): Method = {
    println("Create Method: " + d.name)
    TypeUtils.createMethod(currentScope, d)
  }

  private def createField(v: ValDef): Field = {
    println("Create Field: " + v.name)
    val ctx = Context(currentScope.currentFile, v.pos.line)
    val f: Field = Field(ctx, TypeUtils.getModifiers(v.mods), v.name.toString, TypeUtils.findType(currentScope, v.tpt))
    if (v.mods.hasFlag(Flag.PARAMACCESSOR)) {
      // we need to add the paramaccessors
      // we need to know the information when applying the super constructor
      currentScope.add(f.asIdentifier)
    }
    f
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