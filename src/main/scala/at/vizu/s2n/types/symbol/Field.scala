package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Field(ctx: Context, mods: Seq[Modifier], name: String, tpe: TType) extends Modifiable with Nameable {
  override def modifiers: Set[Modifier] = Set() ++ mods

  def asIdentifier: Identifier = Identifier(ctx, name, tpe, isMutable)
}
