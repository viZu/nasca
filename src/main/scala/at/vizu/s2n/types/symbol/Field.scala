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

  def isProperty = isPublic //&& isParamAccessor // needed for creating getter/setter

  override def toString: String = {
    val v = if (isMutable) "var" else "val"
    val className = if (tpe == null) "" else s": ${tpe.fullClassName}"
    s"$v $name$className"
  }
}
