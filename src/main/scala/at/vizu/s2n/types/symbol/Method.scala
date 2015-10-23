package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Method(ctx: Context, name: String, returnType: Type, mods: Seq[Modifier], params: Seq[Param] = Seq(), constructor: Boolean = false)
  extends Modifiable with Nameable {

  def checkArgs(argsToCheck: Seq[Type]) = {
    false
  }

  override def modifiers: Set[Modifier] = Set() ++ mods
}
