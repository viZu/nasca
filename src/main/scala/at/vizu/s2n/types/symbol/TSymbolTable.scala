package at.vizu.s2n.types.symbol

import at.vizu.s2n.error.TypeErrors
import at.vizu.s2n.generator.GeneratorContext
import at.vizu.s2n.generator.expression.Expression
import at.vizu.s2n.log.Profiler._
import at.vizu.s2n.log.Trace
import at.vizu.s2n.types.symbol.TypeUtils._
import com.typesafe.scalalogging.LazyLogging

/**
 * Phil on 07.10.15.
 */
class TSymbolTable(private var parent: Option[TSymbolTable] = None, private val _this: Option[TType] = None,
                   private var _currentPackage: Option[String] = None, private var _currentFile: Option[String] = None,
                   private val _baseTypes: Option[BaseTypes] = None, private val scopeType: ScopeType = BlockScope) extends LazyLogging {

  private var _types: Seq[TType] = Vector()
  private var _objects: Seq[TType] = Vector()
  private var _appliedTypes: Seq[(Map[TypeArgument, TType], TType, AppliedGenericType)] = Vector()
  private var _identifiers: Seq[Identifier] = Vector()
  private var _typeAliases: Map[String, String] = Map()
  private var _methods: Seq[Method] = Vector()

  def types = _types

  def objects = _objects

  def currentPackage_=(pkg: String) = this._currentPackage = Some(pkg)

  def currentPackage = findCurrentPackage.getOrElse("")

  private def findCurrentPackage: Option[String] = {
    this._currentPackage.map(s => s).orElse(parent.flatMap(_.findCurrentPackage))
  }

  def currentFile_=(file: String) = this._currentFile = Some(file)

  def currentFile: String = findCurrentFile.get

  private def findCurrentFile: Option[String] = {
    this._currentFile.map(s => s).orElse(parent.flatMap(_.findCurrentFile))
  }

  // TODO: Optimize?
  def isEmptyScope: Boolean = (_types ++ _objects ++ _identifiers ++ _methods).isEmpty

  def enterScope(scopeType: ScopeType): TSymbolTable = TSymbolTable(this, scopeType)

  def enterScope(_this: TType): TSymbolTable = TSymbolTable(this, _this)

  def exitScope(): TSymbolTable = {
    val parent = this.parent

    // cleanup for garbage collector ?
    //this.parent = null
    //this._types = null
    //this._objects = null
    //this._identifiers = null
    //this._typeAliases = null
    //this._currentFile = null
    //this._currentPackage = null
    parent.get //Throw exception if parent == None
  }

  /**
   * Types
   */

  def addClass(tpe: TType) = {
    def throwExists() = {
      TypeErrors.addError(tpe.ctx, s"Class with qualifier ${tpe.fullClassName} already exists")
    }
    tpe match {
      case g: TypeArgument =>
        addClassInternal(tpe, findGeneric(_).isEmpty)
      case _ =>
        addClassInternal(tpe, findClassInCurrentScope(_).isEmpty)
    }
  }

  private def addClassInternal(tpe: TType, checkType: String => Boolean) = {
    def throwExists() = {
      TypeErrors.addError(tpe.ctx, s"Class with qualifier ${tpe.fullClassName} already exists")
    }
    if (checkType(tpe.fullClassName)) {
      addNullTypeAsSubType(tpe)
      _types = _types :+ tpe
    } else {
      throwExists()
    }
  }

  def addAllClasses(tpes: Seq[TType]) = _types = _types ++ tpes

  def findAllClasses(): Seq[TType] = {
    _types ++ parent.map(_.findAllClasses()).getOrElse(Vector())
  }

  def findGeneric(name: String): Option[TType] = {
    if (scopeType == ClassScope) _types.find(_.fullClassName == name)
    else _types.find(_.fullClassName == name) orElse parent.flatMap(_.findGeneric(name))
  }

  def findClass(name: String): Option[TType] = {
    findClassWithAlias(name) orElse findClassWithCurrentPackage(name)
  }

  def findClassInCurrentScope(name: String) = _types.find(_.fullClassName == name)

  private def findClassWithName(name: String): Option[TType] = {
    _types.find(_.fullClassName == name) orElse parent.flatMap(_.findClassWithName(name)) //findClassInParent(name)
  }

  private def findClassWithAlias(name: String): Option[TType] = {
    val alias: String = getTypeAlias(name)
    _types.find(_.fullClassName == alias) orElse parent.flatMap(_.findClassWithAlias(alias))
  }

  private def findClassWithCurrentPackage(name: String): Option[TType] = {
    findCurrentPackage match {
      case Some(pkg) =>
        val withPackage = pkg + "." + name
        findClassWithName(withPackage)
      //_types.find(_.fullClassName == withPackage) orElse parent.flatMap(_.findClassWithCurrentPackage())findClassInParent(withPackage)
      case None => None
    }
  }

  private def findClassInParent(name: String): Option[TType] = {
    parent.flatMap(_.findClass(name))
  }

  def addAppliedType(appliedTypes: Map[TypeArgument, TType], onType: TType, result: AppliedGenericType): Unit = {
    _appliedTypes = _appliedTypes :+(appliedTypes, onType, result)
  }

  def findAppliedType(appliedTypes: Map[TypeArgument, TType], onType: TType): Option[AppliedGenericType] = {
    _appliedTypes.find(tpl => tpl._1 == appliedTypes && tpl._2 == onType).map(_._3)
  }

  /**
   * Objects
   */

  def addObject(obj: TType) = {
    if (findObjectInCurrentScope(obj.fullClassName).isEmpty) {
      _objects = _objects :+ obj
    } else {
      TypeErrors.addError(obj.ctx, s"Object with qualifier ${obj.fullClassName} already exists")
    }
  }

  def findObject(name: String): Option[TType] = {
    findObjectWithName(name) orElse findObjectWithAlias(name) orElse findObjectWithCurrentPackage(name)
  }

  def findObjectInCurrentScope(name: String): Option[TType] = {
    _objects.find(_.fullClassName == name)
  }

  private def findObjectWithName(name: String): Option[TType] = {
    _objects.find(_.fullClassName == name) orElse findObjectInParent(name)
  }

  private def findObjectWithAlias(name: String): Option[TType] = {
    val alias: String = getTypeAlias(name)
    _objects.find(_.fullClassName == alias) orElse parent.flatMap(_.findObjectWithAlias(alias))
  }

  private def findObjectInParent(name: String): Option[TType] = {
    parent.flatMap(_.findObject(name))
  }

  private def findObjectWithCurrentPackage(name: String): Option[TType] = {
    findCurrentPackage match {
      case Some(pkg) =>
        val withPackage = pkg + "." + name
        _objects.find(_.fullClassName == withPackage) orElse findObjectInParent(withPackage)
      case None => None
    }
  }


  /**
   * Identifiers
   */

  def add(identifier: Identifier) = {
    if (findIdentifierInCurrentScope(identifier.name).isEmpty) {
      _identifiers = _identifiers :+ identifier
    } else {
      TypeErrors.addError(identifier.ctx,
        s"Identifier with qualifier ${identifier.name} already exists in current scope")
    }
  }

  def addAllIdentifiers(identifiers: Seq[Identifier]) = this._identifiers = this._identifiers ++ identifiers


  def findIdentifier(name: String): Option[Identifier] = {
    findIdentifierWithName(name) orElse findIdentifierWithAlias(name)
  }

  def findIdentifierWithName(name: String): Option[Identifier] = {
    findIdentifierInCurrentScope(name) orElse findIdentifierInParent(name)
  }

  /**
    * For finding object identifier
    *
    * @param name
    * @return
    */
  def findIdentifierWithAlias(name: String): Option[Identifier] = {
    val alias: String = getTypeAlias(name)
    findIdentifierInCurrentScope(alias) orElse findIdentifierInParent(alias)
  }

  def findIdentifierInParent(name: String) = parent.flatMap(_.findIdentifier(name))

  def findIdentifierInCurrentScope(name: String): Option[Identifier] = _identifiers.find(_.name == name)

  /**
   * This
   */

  def findThis(): TType = {
    _this match {
      case Some(tpe) => tpe
      case None if parent.isDefined => parent.get.findThis()
      case _ => throw new RuntimeException("No this found for current scope")
    }
  }

  /**
   * Type aliases
   */

  def addTypeAlias(alias: String, aliasFor: String) = {
    _typeAliases = _typeAliases + (alias -> aliasFor) + (ScalaPackage + "." + alias -> aliasFor)
  }

  private def getTypeAlias(name: String): String = {
    _typeAliases.getOrElse(name, name)
  }

  /**
   * Methods in scope
   */

  def addMethod(method: Method) = {
    val args: Seq[TType] = method.params.map(_.tpe)
    if (findMethodInScopeMethods(method.name, args).isEmpty) {
      _methods = _methods :+ method
    } else {
      TypeErrors.addError(method.ctx,
        s"Method ${method.name}(${TypeUtils.toString(args)}) already exists in current scope")
    }
  }

  def findMethod(name: String, args: Seq[TType]): Option[Method] = {
    findMethodInScopeMethods(name, args) orElse findMethodInThis(name, args) orElse findApply(name, args)
  }

  private def findMethodInScopeMethods(name: String, args: Seq[TType]): Option[Method] = {
    //TODO check return type too?
    _methods.find(m => m.name == name && m.checkArgsSuperType(args)) orElse findMethodInScopeMethodsInParent(name, args)
  }

  private def findMethodInThis(name: String, args: Seq[TType]): Option[Method] = {
    val thisTpe: TType = findThis()
    thisTpe.findMethod(thisTpe, name, args)
  }

  private def findApply(name: String, args: Seq[TType]): Option[Method] = {
    val thisTpe = findThis()
    profile(logger, "findApply-findObjectWithAlias", findObjectWithAlias(name), Trace) match {
      case Some(tpe) => tpe.findApply(thisTpe, args)
      case None => profile(logger, "findApply-findIdentifier", findIdentifier(name), Trace) match {
        case Some(i) => i.tpe.findApply(thisTpe, args)
        case None => thisTpe.findField(thisTpe, name) match {
          case Some(f) => f.tpe.findApply(thisTpe, args)
          case None => None
        }
      }
    }
  }

  private def findMethodInScopeMethodsInParent(name: String, args: Seq[TType]): Option[Method] = {
    parent.flatMap(_.findMethodInScopeMethods(name, args))
  }

  /**
    * Null Type
    */

  private def findNullType(): TType = {
    findClass(RootScalaPackage + ".Null").get
  }

  private def addNullTypeAsSubType(tpe: TType): Unit = {
    findNullType().asInstanceOf[ConcreteType].addParent(Parent(tpe))
  }

  def baseTypes: BaseTypes = {
    _baseTypes match {
      case Some(b) => b
      case None if parent.isDefined => parent.get.baseTypes
      case _ => throw new RuntimeException("No basetypes found")
    }
  }

  def getRootScope: TSymbolTable = {
    parent match {
      case None => this
      case Some(s) => s.getRootScope
    }
  }

  def getNonBaseTypes: Seq[TType] = {
    val myBaseTypes = baseTypes
    types.filter(!myBaseTypes.isBaseType(_))
  }

  def getNonBaseObjects: Seq[TType] = {
    val myBaseTypes = baseTypes
    objects.filter(!myBaseTypes.isBaseType(_))
  }

  def isRootScope: Boolean = parent.isEmpty


  /**
    * Scoped functions, for entering new scopes
    */

  def scoped(f: TSymbolTable => Option[TType], scopeType: ScopeType): Option[TType] = {
    val childScope: TSymbolTable = enterScope(scopeType)
    val tpe = f(childScope)
    childScope.exitScope()
    tpe
  }

  def scoped(f: TSymbolTable => TType, scopeType: ScopeType): TType = {
    val childScope: TSymbolTable = enterScope(scopeType)
    val tpe = f(childScope)
    childScope.exitScope()
    tpe
  }

  def scoped[U](f: TSymbolTable => U, scopeType: ScopeType): U = {
    val childScope: TSymbolTable = enterScope(scopeType)
    val ret = f(childScope)
    childScope.exitScope()
    ret
  }

  def scoped(f: TSymbolTable => Expression, scopeType: ScopeType): Expression = {
    val childScope: TSymbolTable = enterScope(scopeType)
    val expr = f(childScope)
    childScope.exitScope()
    expr
  }

  def scoped(f: TSymbolTable => GeneratorContext, scopeType: ScopeType): GeneratorContext = {
    val childScope: TSymbolTable = enterScope(scopeType)
    val generated = f(childScope)
    childScope.exitScope()
    generated
  }
}

object TSymbolTable {
  def apply() = new TSymbolTable(scopeType = RootScope)

  def apply(baseTypes: BaseTypes) = new TSymbolTable(_baseTypes = Option(baseTypes), scopeType = RootScope)

  def apply(parent: TSymbolTable, scopeType: ScopeType) = new TSymbolTable(Option(parent), scopeType = scopeType)

  def apply(parent: TSymbolTable, _this: TType) = new TSymbolTable(Option(parent), Option(_this), scopeType = ClassScope)
}

trait ScopeType

object RootScope extends ScopeType

object ClassScope extends ScopeType

object MethodScope extends ScopeType

object BlockScope extends ScopeType