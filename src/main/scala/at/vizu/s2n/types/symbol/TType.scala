package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException

/**
 * Phil on 07.10.15.
 */

case class TType(ctx: Context = Context("", 0), simpleName: String,
                 pkg: String = "", mods: Seq[Modifier] = Seq(), isObject: Boolean = false) extends Modifiable with Nameable {

  private var _methods: Seq[Method] = Seq()
  private var _fields: Seq[Field] = Seq()
  var parents: Seq[TType] = Seq()
  var generics: Seq[GenericModifier] = Seq()

  lazy val modifiers: Set[Modifier] = Set() ++ mods

  def name = fullClassName

  def methods = _methods

  def fields = _fields

  def findMethod(name: String, args: Seq[TType]): Option[Method] = {
    findMethodInSelf(name, args) orElse findMethodInSelfWithSuper(name, args) orElse findMethodInParents(name, args)
  }

  private def findMethodInSelf(name: String, args: Seq[TType]): Option[Method] = {
    _methods.find(m => m.name == name && m.checkArgs(args))
  }

  private def findMethodInSelfWithSuper(name: String, args: Seq[TType]): Option[Method] = {
    _methods.find(m => m.name == name && m.checkArgsSuperType(args))
  }

  private def findMethodInParents(name: String, args: Seq[TType]): Option[Method] = {
    val optMethods: Seq[Option[Method]] = parents.map(_.findMethod(name, args)).filter(_.isDefined)
    if (optMethods.nonEmpty) optMethods.head else None
  }

  def findField(name: String): Option[Field] = {
    _fields.find(_.name == name) orElse findFieldInParents(name)
  }

  private def findFieldInParents(name: String): Option[Field] = {
    val optFields: Seq[Option[Field]] = parents.map(_.findField(name)).filter(_.isDefined)
    if (optFields.nonEmpty) optFields.head else None
  }

  def hasParent(tpe: TType): Boolean = {
    if (tpe == this) true
    else parents.exists(_.hasParent(tpe))
  }

  def forEachType(f: TType => Unit): Unit = {
    f(this)
    parents.foreach(_.forEachType(f))
  }

  def fullClassName = if (pkg.isEmpty) simpleName else pkg + "." + simpleName

  def addMethod(method: Method) = {
    val args: Seq[TType] = method.params.map(_.tpe)
    val methodFound = findMethod(method.name, args).isDefined
    if (!methodFound || methodFound && method.isOverride) {
      _methods = _methods :+ handleConstructor(method)
    } else {
      throw new TypeException(method.ctx.fileName, method.ctx.line,
        s"Type $fullClassName already has a method ${method.name}(${TypeUtils.toString(args)})")
    }
  }

  private def handleConstructor(m: Method) = {
    if (m.constructor) Method(m.ctx, m.name, this, m.mods, m.params, m.constructor)
    else m
  }

  private def validateMethod(method: Method) = {
    if (method.isAbstract && !isAbstract && !isTrait) {
      throw new TypeException(method.ctx.fileName, method.ctx.line,
        s"Type $fullClassName must either be abstract or implement abstract member ${method.name}")
    }
  }

  def addField(field: Field) = {
    if (findField(field.name).isEmpty) {
      _fields = _fields :+ field
    } else {
      throw new TypeException(field.ctx.fileName, field.ctx.line,
        s"Type $fullClassName already has a field with name ${field.name}")
    }
  }

  private def validateField(field: Field) = {
    if (field.isAbstract && !isAbstract && !isTrait) {
      throw new TypeException(field.ctx.fileName, field.ctx.line,
        s"Type $fullClassName must either be abstract or implement abstract member ${field.name}")
    }
  }

  def addParent(parent: TType) = {
    parents = parents :+ parent
  }

  private def validateParent(p: (TType, Int)) = {
    val (parent, index) = p
    if (index > 0 && !parent.isTrait && !isNullType) {
      throw new TypeException(ctx.fileName, ctx.line, s"Type ${parent.name} needs to be a trait to be mixed in")
    }
  }

  def validate() = {
    _methods.foreach(validateMethod)
    _fields.foreach(validateField)
    parents.zipWithIndex.foreach(validateParent)
  }

  private def isNullType = "scala.Null" == fullClassName

  override def toString = fullClassName
}