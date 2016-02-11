package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
trait TType extends Modifiable with Identifiable {

  def ctx: Context

  def simpleName: String

  def pkg: String

  def mods: Seq[Modifier]

  def isObject: Boolean

  def name: String = fullClassName

  def tpe: TType = this

  def methods: Seq[Method]

  def fields: Seq[Field]

  lazy val members = methods ++ fields
  lazy val modifiers: Set[Modifier] = Set() ++ mods

  def parents: Seq[Parent]

  def parentTypes: Seq[TType] = parents.map(_.tpe)

  def findMethod(execCtx: TType, name: String, args: Seq[TType]): Option[Method]

  def findApply(execCtx: TType, args: Seq[TType]) = findMethod(execCtx, TypeUtils.ApplyMethodName, args)

  def findConstructor(execCtx: TType, args: Seq[TType]) = findMethod(execCtx, TypeUtils.ConstructorName, args)

  def findField(execCtx: TType, name: String): Option[Field]

  def hasParent(tpe: TType): Boolean

  def isAssignableFrom(other: TType): Boolean = other.hasParent(this)

  def isAssignableAsParam(other: TType): Boolean

  def foreachType(f: TType => Unit): Unit = {
    f(this)
    parents.map(_.tpe).foreach(_.foreachType(f))
  }
  def fullClassName = if (pkg.isEmpty) simpleName else pkg + "." + simpleName

  def validate(): Unit

  def typeEquals(obj: Any): Boolean = this.equals(obj)

  def baseTypeEquals(obj: TType): Boolean

}

object EmptyType extends TType {
  override def ctx: Context = Context("", 0)

  override def isObject: Boolean = false

  override def methods: Seq[Method] = Seq()

  override def pkg: String = ""

  override def simpleName: String = "notype"

  override def hasParent(tpe: TType): Boolean = false

  override def fields: Seq[Field] = Seq()

  override def validate(): Unit = return

  override def findField(execCtx: TType, name: String): Option[Field] = None

  override def mods: Seq[Modifier] = Seq()

  override def isAssignableAsParam(other: TType): Boolean = false

  override def parents: Seq[Parent] = Seq()

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]): Option[Method] = None

  override def baseTypeEquals(obj: TType): Boolean = obj == this
}