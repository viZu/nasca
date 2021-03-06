package at.vizu.s2n.types.symbol

/**
 * Phil on 06.11.15.
 */
trait BaseTypes {

  val any: TType
  val anyVal: TType
  val anyRef: TType
  val string: TType
  val numeric: TType
  val unit: TType
  val boolean: TType
  val byte: TType
  val short: TType
  val char: TType
  val int: TType
  val long: TType
  val float: TType
  val double: TType
  val nullTpe: TType
  val nothing: TType
  val array: TType

  val primitives: Set[TType]

  def isPrimitive(tpe: TType) = tpe match {
    case c: ConcreteType => primitives.contains(c)
    case a: AppliedTypeArgument => primitives.contains(a.getConcreteType)
    case _ => false
  }

  def isBaseType(tpe: TType) = tpe.pkg.startsWith(TypeUtils.RootScalaPackage)

}
