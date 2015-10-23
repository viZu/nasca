package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
class Scope(private var parent: Scope = null, private val _this: Option[Type] = None) {

  private var _types: Seq[Type] = Vector()
  private var _objects: Seq[Type] = Vector()
  private var _identifiers: Seq[Identifier] = Vector()
  private var _typeAliases: Map[String, String] = Map()
  private var _methods: Seq[Method] = Vector()

  def types = _types

  def objects = _objects

  def enterScope(): Scope = Scope(this)

  def enterScope(_this: Type): Scope = Scope(this)

  def exitScope(): Scope = {
    val parent = this.parent

    // cleanup for garbage collector ?
    this.parent = null
    this._types = null
    this._objects = null
    this._identifiers = null
    this._typeAliases = null
    parent
  }

  private def add[T <: Nameable](toAdd: T, addMethod: T => Unit, findMethod: String => Option[T]) = {
    findMethod(toAdd.name) match {
      case None => addMethod(toAdd)
      case Some(_) => throw new RuntimeException(s"${toAdd.getClass.getSimpleName} with name ${toAdd.name} already exists")
    }
  }

  /**
   * Types
   */

  def addClass(tpe: Type) = _types = _types :+ tpe

  def addAllClasses(tpes: Seq[Type]) = _types = _types ++ tpes

  def findClass(name: String): Option[Type] = {
    findClassWithName(name) orElse findClassWithAlias(name)
  }

  private def findClassWithName(name: String): Option[Type] = {
    _types.find(_.fullClassName == name) orElse findClassInParent(name)
  }

  private def findClassWithAlias(name: String): Option[Type] = {
    val alias: String = getTypeAlias(name)
    _types.find(_.fullClassName == alias) orElse findClassInParent(alias)
  }

  private def findClassInParent(name: String): Option[Type] = {
    if (parent != null) parent.findClass(name) else None
  }

  /**
   * Objects
   */

  def addObject(obj: Type) = _objects = _objects :+ obj

  def findObject(name: String): Option[Type] = {
    findObjectWithName(name) orElse findObjectWithAlias(name)
  }

  private def findObjectWithName(name: String): Option[Type] = {
    _objects.find(_.fullClassName == name) orElse findObjectInParent(name)
  }

  private def findObjectWithAlias(name: String): Option[Type] = {
    val alias: String = getTypeAlias(name)
    _objects.find(_.fullClassName == alias) orElse findObjectInParent(alias)
  }

  private def findObjectInParent(name: String): Option[Type] = {
    if (parent != null) parent.findObject(name) else None
  }


  /**
   * Identifiers
   */

  def add(identifier: Identifier) = _identifiers = _identifiers :+ identifier

  def addAllIdentifiers(identifiers: Seq[Identifier]) = this._identifiers = this._identifiers ++ identifiers

  def findIdentifier(name: String): Option[Identifier] = _identifiers.find(_.name == name)

  /**
   * This
   */

  def findThis(): Type = {
    _this match {
      case Some(tpe) => tpe
      case None if parent != null => parent.findThis()
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

  def addMethod(method: Method) = _methods = _methods :+ method

  def findMethod(name: String, args: Seq[Type]): Method = {
    val m = findMethodInScopeMethods(name, args) orElse findMethodInThis(name, args) orElse findApply(name, args)
    m.get
  }

  private def findMethodInScopeMethods(name: String, args: Seq[Type]): Option[Method] = {
    _methods.find(_.checkArgs(args))
  }

  private def findMethodInThis(name: String, args: Seq[Type]): Option[Method] = {
    _this match {
      case Some(tpe) => tpe.findMethod(name, args)
      case None => None
    }
  }

  private def findApply(name: String, args: Seq[Type]): Option[Method] = {
    findClassWithAlias(name) match {
      case Some(tpe) => tpe.findMethod("apply", args)
      case None => None
    }
  }

  /**
   * Check scope
   */

  def checkScope(identifier: Identifier) = findIdentifier(identifier.name).isEmpty

  def checkScope(tpe: Type) = findClass(tpe.fullClassName).isEmpty

}

object Scope {
  def apply(parent: Scope) = new Scope(parent)

  def apply(parent: Scope, _this: Type) = new Scope(parent, Some(_this))
}
