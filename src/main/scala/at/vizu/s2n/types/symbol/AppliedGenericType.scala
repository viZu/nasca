package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class AppliedGenericType(val appliedTypes: Seq[TType],
                         val genericType: GenericType) extends GenericType(
  genericType.ctx, genericType.simpleName, genericType.pkg, genericType.mods, genericType.isObject) {

  genericType.memberAddedListener += memberAddedInGenericType

  private val appliedTypeMap = genericType.genericModifiers.zip(appliedTypes).toMap

  private def memberAddedInGenericType(member: Member) = {
    member match {
      case f: Field => addField(mapField(appliedTypeMap, f, this))
      case m: Method => addMethod(mapMethod(appliedTypeMap, m, this))
    }
  }

  override def genericModifiers: Seq[GenericModifier] = genericType.genericModifiers

  override private[symbol] def parents: Seq[TType] = genericType.parents

  override def toString: String = {
    val typeString = appliedTypes.map(_.toString).mkString(", ")
    s"$name[$typeString]"
  }

  override def equals(that: scala.Any): Boolean = {
    that match {
      case a: AppliedGenericType =>
        appliedTypes == a.appliedTypes && genericType == a.genericType
      case g: GenericType =>
        g == this
      case _ => false
    }
  }

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case a: AppliedGenericType =>
      a.genericType == this.genericType &&
        genericModifiers.zip(a.appliedTypes).forall({ case (gm, tpe) => gm.isAssignableFrom(tpe) })
    case _ => false
  }
}
