package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Method(ctx: Context, name: String, returnType: TType, mods: Seq[Modifier], params: Seq[Param] = Seq(),
                  constructor: Boolean = false, instanceMethod: Boolean = true)
  extends Modifiable with Nameable {

  def checkArgs(argsToCheck: Seq[TType]) = {
    params.map(_.tpe) == argsToCheck
  }

  def checkArgsSuperType(argsToCheck: Seq[TType]) = {
    val filtered: Seq[Boolean] = argsToCheck.zipWithIndex.map(a => a._1.hasParent(params(a._2).tpe)).filter(_ == true)
    filtered.size == params.size // TODO optional params
  }

  lazy val modifiers: Set[Modifier] = Set() ++ mods

  override def toString = {
    val params = this.params.map(_.tpe)
    s"$name(${TypeUtils.toString(params)})"
  }
}
