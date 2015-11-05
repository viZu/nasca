package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException

/**
 * Phil on 07.10.15.
 */
class TScope(private var parent: Option[TScope] = None, private val _this: Option[TType] = None,
             private var _currentPackage: Option[String] = None, private var _currentFile: Option[String] = None) {

  private var _types: Seq[TType] = Vector()
  private var _objects: Seq[TType] = Vector()
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

  def enterScope(): TScope = TScope(this)

  def enterScope(_this: TType): TScope = TScope(this, _this)

  def exitScope(): TScope = {
    val parent = this.parent

    // cleanup for garbage collector ?
    this.parent = null
    this._types = null
    this._objects = null
    this._identifiers = null
    this._typeAliases = null
    this._currentFile = null
    this._currentPackage = null
    parent.get //Throw exception if parent == None
  }

  /**
   * Types
   */

  def addClass(tpe: TType) = {
    if (findClassInCurrentScope(tpe.fullClassName).isEmpty) {
      _types = _types :+ tpe
    } else {
      throw new TypeException(tpe.ctx.fileName, tpe.ctx.line,
        s"Class with qualifier ${tpe.fullClassName} already exists")
    }
  }

  def addAllClasses(tpes: Seq[TType]) = _types = _types ++ tpes

  def findClass(name: String): Option[TType] = {
    findClassWithName(name) orElse findClassWithAlias(name) orElse findClassWithCurrentPackage(name)
  }

  def findClassInCurrentScope(name: String) = _types.find(_.fullClassName == name)

  private def findClassWithName(name: String): Option[TType] = {
    _types.find(_.fullClassName == name) orElse findClassInParent(name)
  }

  private def findClassWithAlias(name: String): Option[TType] = {
    val alias: String = getTypeAlias(name)
    _types.find(_.fullClassName == alias) orElse findClassInParent(alias)
  }

  private def findClassWithCurrentPackage(name: String): Option[TType] = {
    findCurrentPackage match {
      case Some(pkg) =>
        val withPackage = pkg + "." + name
        _types.find(_.fullClassName == withPackage) orElse findClassInParent(withPackage)
      case None => None
    }
  }

  private def findClassInParent(name: String): Option[TType] = {
    parent.flatMap(_.findClass(name))
  }

  /**
   * Objects
   */

  def addObject(obj: TType) = {
    if (findObjectInCurrentScope(obj.fullClassName).isEmpty) {
      _objects = _objects :+ obj
    } else {
      throw new TypeException(obj.ctx.fileName, obj.ctx.line,
        s"Object with qualifier ${obj.fullClassName} already exists")
    }
  }

  def findObject(name: String): Option[TType] = {
    findObjectWithName(name) orElse findObjectWithAlias(name)
  }

  def findObjectInCurrentScope(name: String): Option[TType] = {
    _objects.find(_.fullClassName == name)
  }

  private def findObjectWithName(name: String): Option[TType] = {
    _objects.find(_.fullClassName == name) orElse findObjectInParent(name)
  }

  private def findObjectWithAlias(name: String): Option[TType] = {
    val alias: String = getTypeAlias(name)
    _objects.find(_.fullClassName == alias) orElse findObjectInParent(alias)
  }

  private def findObjectInParent(name: String): Option[TType] = {
    parent.flatMap(_.findObject(name))
  }


  /**
   * Identifiers
   */

  def add(identifier: Identifier) = {
    if (findIdentifierInCurrentScope(identifier.name).isEmpty) {
      _identifiers = _identifiers :+ identifier
    } else {
      throw new TypeException(identifier.ctx.fileName, identifier.ctx.line,
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

  def addTypeAlias(alias: String, aliasFor: String) = _typeAliases = _typeAliases + (alias -> aliasFor)

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
      throw new TypeException(method.ctx,
        s"Method ${method.name}(${TypeUtils.toString(args)}) already exists in current scope")
    }
  }

  def findMethod(name: String, args: Seq[TType]): Option[Method] = {
    findMethodInScopeMethods(name, args) orElse findMethodInThis(name, args) orElse findApply(name, args)
  }

  private def findMethodInScopeMethods(name: String, args: Seq[TType]): Option[Method] = {
    //TODO check return type too?
    _methods.find(m => m.name == name && m.checkArgs(args))
  }

  private def findMethodInThis(name: String, args: Seq[TType]): Option[Method] = {
    findThis().findMethod(name, args)
  }

  private def findApply(name: String, args: Seq[TType]): Option[Method] = {
    findObjectWithAlias(name) match {
      case Some(tpe) => tpe.findMethod("apply", args)
      case None => None
    }
  }

  /**
    * Null Type
    */

  private def findNullType(): TType = {
    findClass("scala.Null").get
  }

  private def addNullTypeAsSubType(tpe: TType): Unit = {
    findNullType().addParent(tpe)
  }

  /**
   * Check scope
   */

  //def checkScope(identifier: Identifier) = findIdentifier(identifier.name).isEmpty

  //def checkScope(tpe: Type) = findClass(tpe.fullClassName).isEmpty

}

object TScope {
  def apply(parent: TScope) = new TScope(Some(parent))

  def apply(parent: TScope, _this: TType) = new TScope(Some(parent), Some(_this))
}
