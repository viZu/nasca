package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Method(ctx: Context, name: String, returnType: TType, mods: Seq[Modifier], params: Seq[Param] = Seq(), constructor: Boolean = false)
  extends Modifiable with Nameable {

  def checkArgs(argsToCheck: Seq[TType]) = {
    params.map(_.tpe) == argsToCheck
  }

  def checkArgsSuperType(argsToCheck: Seq[TType]) = {
    val filtered: Seq[Boolean] = argsToCheck.zipWithIndex.map(a => a._1.hasParent(params(a._2).tpe)).filter(_ == true)
    filtered.size == argsToCheck.size // TODO optional params
  }

  override def modifiers: Set[Modifier] = Set() ++ mods
}
