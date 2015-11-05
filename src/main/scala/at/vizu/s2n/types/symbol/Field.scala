package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
// var tpe for type inference...
case class Field(ctx: Context, mods: Seq[Modifier], name: String, var tpe: TType) extends Modifiable with Nameable {
  lazy val modifiers: Set[Modifier] = Set() ++ mods

  def asIdentifier: Identifier = Identifier(ctx, name, tpe, isMutable)

  override def toString: String = {
    val v = if (isMutable) "var" else "val"
    s"$v name: ${tpe.fullClassName}"
  }
}
