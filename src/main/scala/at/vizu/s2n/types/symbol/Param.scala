package at.vizu.s2n.types.symbol

/**
 * Phil on 15.10.15.
 */
case class Param(ctx: Context, tpe: TType, name: String, hasDefaultVal: Boolean = false, mutable: Boolean = false) extends Nameable {

  override def toString: String = {
    val v = if (mutable) "var" else "val"
    s"$v name: ${tpe.toString}"
  }

}
