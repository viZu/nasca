package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Method(ctx: Context, name: String, returnType: TType, mods: Seq[Modifier], params: Seq[Param] = Vector(),
                  generics: Seq[GenericModifier] = Vector(), constructor: Boolean = false, instanceMethod: Boolean = true,
                  operator: Boolean = false)
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

  def getAppliedTypes(args: Seq[TType]) = {
    require(args.size == params.size)
    params.map(_.tpe).zip(args).flatMap(extractAppliedType).toMap
  }

  private def extractAppliedType(paramToArg: (TType, TType)): Seq[(GenericModifier, TType)] = {
    paramToArg match {
      //TODO: check if arg == generic modifier && != generic modifier
      //case (agm: AppliedGenericModifier, _) => Vector()
      case (gmParam: GenericModifier, atArg: GenericModifier) => Vector(gmParam -> atArg)
      case (gmParam: GenericModifier, ctArg: ConcreteType) => Vector(gmParam -> ctArg)
      case (atParam: AppliedGenericType, atArg: AppliedGenericType) =>
        atParam.appliedTypes.zip(atArg.appliedTypes).flatMap(extractAppliedType)
      case (gtParam: GenericType, atArg: AppliedGenericType) =>
        gtParam.genericModifiers.zip(atArg.appliedTypes)
      case (_, _) => Vector()
    }
  }

  def applyTypes(types: Map[GenericModifier, TType]) = {
    val retTpe = TypeUtils.getNewTpe(types, returnType, applyPartly = false)
    val newGenerics = generics.map(TypeUtils.getNewTpe(types, _, applyPartly = false))
      .collect({ case gt: GenericModifier if !gt.isInstanceOf[AppliedGenericModifier] => gt })
    val newParams = applyTypesOnParams(types)
    Method(ctx, name, retTpe, mods, newParams, newGenerics, constructor, instanceMethod, operator)
  }

  private def applyTypesOnParams(types: Map[GenericModifier, TType]) = {
    params.map(_.applyTypes(types))
  }

  override def toString = {
    val params = this.params.map(_.tpe)
    val gstr = if (generics.nonEmpty) s"""[${generics.map(_.name).mkString(", ")}]""" else ""
    if (constructor) constructorString(params, gstr) else methodString(params, gstr)
  }

  private def constructorString(params: Seq[TType], genericString: String) = {
    s"$returnType$genericString(${TypeUtils.toString(params)})"
  }

  private def methodString(params: Seq[TType], genericString: String) = {
    s"$name$genericString(${TypeUtils.toString(params)}): ${returnType.name}"
  }

  override def tpe: TType = returnType
}
