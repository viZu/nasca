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

  val primitives: Set[TType]

  def isPrimitive(tpe: TType) = primitives.contains(tpe)

}
