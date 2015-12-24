package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException
import at.vizu.s2n.generator.expression.Expression

import scala.collection.mutable.ArrayBuffer

/**
  * Phil on 07.10.15.
  */

class ConcreteType(_ctx: Context = Context("", 0), _simpleName: String,
                   _pkg: String = "", _mods: Seq[Modifier] = Vector(), private[symbol] val _isObject: Boolean = false) extends TType {

  protected var _methods: Seq[Method] = Vector()
  protected var _fields: Seq[Field] = Vector()
  private[symbol] var memberAddedListener: ArrayBuffer[Member => Unit] = ArrayBuffer()

  def parents = _parents

  private[symbol] var _parents: Seq[Parent] = Vector()

  def methods = _methods

  def fields = _fields

  def findMethod(execCtx: TType, name: String, args: Seq[TType]): Option[Method] = {
    findMethodInSelf(execCtx, name, args) orElse
      findMethodInSelfWithSuper(execCtx, name, args) orElse
      findMethodInParents(execCtx, name, args)
  }

  private def findMethodInSelf(execCtx: TType, name: String, args: Seq[TType]): Option[Method] = {
    _methods.find(m => checkMethod(execCtx, name, m) && m.checkArgs(args))
  }

  private def findMethodInSelfWithSuper(execCtx: TType, name: String, args: Seq[TType]): Option[Method] = {
    _methods.find(m => checkMethod(execCtx, name, m) && m.checkArgsSuperType(args))
  }

  private def checkMethod(execCtx: TType, name: String, method: Method) = {
    method.name == name && checkVisibility(execCtx, method)
  }

  private def findMethodInParents(execCtx: TType, name: String, args: Seq[TType]): Option[Method] = {
    val optMethods: Seq[Option[Method]] = parentTypes.map(_.findMethod(execCtx, name, args)).filter(_.isDefined)
    if (optMethods.nonEmpty) optMethods.head else None
  }

  def findField(execCtx: TType, name: String): Option[Field] = {
    _fields.find(checkField(execCtx, name, _)) orElse findFieldInParents(execCtx, name)
  }

  private def findFieldInParents(execCtx: TType, name: String): Option[Field] = {
    val optFields: Seq[Option[Field]] = parentTypes.map(_.findField(execCtx, name)).filter(_.isDefined)
    if (optFields.nonEmpty) optFields.head else None
  }

  private def checkField(execCtx: TType, name: String, field: Field) = {
    field.name == name && checkVisibility(execCtx, field)
  }

  private def checkVisibility(execCtx: TType, member: Member) = {
    if (member.isPublic) true
    else if (member.isPrivate) execCtx == this
    else if (member.isProtected) execCtx.hasParent(this)
    else false
  }

  def hasParent(tpe: TType): Boolean = {
    if (tpe == this) true
    else parentTypes.exists(_.hasParent(tpe))
  }

  def addMethod(method: Method) = {
    val args: Seq[TType] = method.params.map(_.tpe)
    val methodFound = findMethod(this, method.name, args).isDefined
    if (!methodFound || methodFound && method.isOverride || methodFound && method.constructor) {
      val addedMethod = handleConstructor(method)
      _methods = _methods :+ addedMethod
      notifyMemberAddedListener(addedMethod)
    } else {
      throw new TypeException(method.ctx.fileName, method.ctx.line,
        s"Type $fullClassName already has a method ${method.name}(${TypeUtils.toString(args)})")
    }
  }

  private def handleConstructor(m: Method) = {
    if (m.constructor) Method(m.ctx, m.name, this, m.mods, m.params, m.generics, m.constructor)
    else m
  }

  private def validateMethod(method: Method) = {
    if (method.isAbstract && !isAbstract && !isTrait) {
      throw new TypeException(method.ctx.fileName, method.ctx.line,
        s"Type $fullClassName must either be abstract or implement abstract member ${method.name}")
    }
    if (!method.constructor) {
      // check only if not constructor
      method.params.foreach(validateParam(method.ctx, _))
      validateReturnType(method)
    }
  }

  private def validateParam(ctx: Context, p: Param) = {
    TypeUtils.findGenericModifiers(p.tpe).foreach(gm => {
      if (gm.covariance) {
        throw new TypeException(ctx.fileName, ctx.line, s"Covariant type ${gm.name} occurs in contravariant position" +
          s" in type ${p.tpe.name} of value ${p.name}")
      }
    })
  }

  private def validateReturnType(m: Method) = {
    val tpe = m.tpe
    val ctx = m.ctx
    TypeUtils.findGenericModifiers(tpe).foreach(gm => {
      if (gm.contravariance) {
        throw new TypeException(ctx.fileName, ctx.line, s"Contravariant type ${gm.name} occurs in covariant position" +
          s" in type ${tpe.name} of method $m")
      }
    })
  }

  def addField(field: Field) = {
    val fieldFound = findField(this, field.name).isDefined
    if (!fieldFound || fieldFound && field.isOverride) {
      _fields = _fields :+ field
      notifyMemberAddedListener(field)
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
    val tpe: TType = field.tpe
    TypeUtils.findGenericModifiers(tpe).foreach(gm => {
      if (gm.contravariance) {
        throw new TypeException(ctx.fileName, ctx.line, s"Contravariant type ${gm.name} occurs in covariant position" +
          s" in type ${tpe.name} of value ${field.name}")
      }
    })
  }

  private def notifyMemberAddedListener(member: Member) = {
    memberAddedListener.foreach(_.apply(member))
  }

  def addParent(parent: Parent) = {
    _parents = _parents :+ parent
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
    parentTypes.zipWithIndex.foreach(validateParent)
  }

  private def isNullType = "scala.Null" == fullClassName

  override def toString = fullClassName

  override def ctx: Context = _ctx

  override def simpleName: String = _simpleName

  override def pkg: String = _pkg

  override def mods: Seq[Modifier] = _mods

  override def isObject: Boolean = _isObject

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case c: ConcreteType => this.isAssignableFrom(c)
    case a: AppliedGenericModifier => this.isAssignableFrom(a.appliedType)
    case _ => false
  }
}

object ConcreteType {

  def apply(ctx: Context = Context("", 0), simpleName: String,
            pkg: String = "", mods: Seq[Modifier] = Vector(), isObject: Boolean = false) = {
    new ConcreteType(ctx, simpleName, pkg, mods, isObject)
  }

}