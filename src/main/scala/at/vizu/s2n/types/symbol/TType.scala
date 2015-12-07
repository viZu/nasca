package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
trait TType extends Modifiable with Nameable {

  def ctx: Context

  def simpleName: String

  def pkg: String

  def mods: Seq[Modifier]

  def isObject: Boolean

  def name: String = fullClassName

  def methods: Seq[Method]

  def fields: Seq[Field]

  lazy val members = methods ++ fields
  lazy val modifiers: Set[Modifier] = Set() ++ mods

  protected def parents: Seq[TType]

  def findMethod(execCtx: TType, name: String, args: Seq[TType]): Option[Method]

  def findField(execCtx: TType, name: String): Option[Field]

  def hasParent(tpe: TType): Boolean

  def isAssignableFrom(other: TType) = other.hasParent(this)

  def foreachType(f: TType => Unit): Unit = {
    f(this)
    parents.foreach(_.foreachType(f))
  }
  def fullClassName = if (pkg.isEmpty) simpleName else pkg + "." + simpleName

  def validate(): Unit
}
