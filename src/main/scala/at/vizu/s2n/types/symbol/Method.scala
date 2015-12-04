package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Method(ctx: Context, name: String, returnType: TType, mods: Seq[Modifier], params: Seq[Param] = Seq(),
                  constructor: Boolean = false, instanceMethod: Boolean = true, operator: Boolean = false)
  extends Member {

  def checkArgs(argsToCheck: Seq[TType]) = {
    params.map(_.tpe) == argsToCheck
  }

  def checkArgsSuperType(argsToCheck: Seq[TType]) = {
    // TODO optional params
    if (argsToCheck.size != params.size) false
    else {
      val definedParams = params.map(_.tpe)
      TypeUtils.areParamsApplicable(definedParams, argsToCheck)
      //val filtered: Seq[Boolean] = argsToCheck.zipWithIndex.map(a => a._1.hasParent(params(a._2).tpe)).filter(_ == true)
      //filtered.size == params.size
    }
  }

  lazy val modifiers: Set[Modifier] = Set() ++ mods

  override def toString = {
    val params = this.params.map(_.tpe)
    s"$name(${TypeUtils.toString(params)})"
  }

  override def tpe: TType = returnType
}
