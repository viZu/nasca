package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
// var tpe for type inference...
case class Field(ctx: Context, mods: Seq[Modifier], name: String, var _tpe: TType) extends Member {

  def tpe = _tpe

  def tpe_=(newTpe: TType) = _tpe = newTpe

  lazy val modifiers: Set[Modifier] = Set() ++ mods

  def asIdentifier: Identifier = Identifier(ctx, name, tpe, isMutable, fromField = true)

  override def toString: String = {
    val v = if (isMutable) "var" else "val"
    s"$v $name: ${tpe.fullClassName}"
  }
}
