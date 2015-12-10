package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class AppliedGenericModifier(val appliedType: TType, val genericType: GenericModifier)
  extends GenericModifier(genericType.ctx, genericType.genericName, genericType.upperBound, genericType.lowerBound,
    genericType.coVariance, genericType.contraVariance) {

  override def methods: Seq[Method] = appliedType.methods

  override def fields: Seq[Field] = appliedType.fields

  override def pkg: String = appliedType.pkg

  override def simpleName: String = appliedType.simpleName

  override def hasParent(tpe: TType): Boolean = appliedType.hasParent(tpe)

  override def isAssignableFrom(other: TType): Boolean = other.hasParent(appliedType)

  override def findField(execCtx: TType, name: String) = appliedType.findField(execCtx, name)

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]) = appliedType.findMethod(execCtx, name, args)

  override private[symbol] def parents: Seq[TType] = appliedType.parents

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case a: AppliedGenericModifier => a == other
  }
}
