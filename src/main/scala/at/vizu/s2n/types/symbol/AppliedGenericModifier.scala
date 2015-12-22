package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class AppliedGenericModifier(val appliedType: TType, genericName: String, upperBound: TType, lowerBound: TType,
                             covariant: Boolean, contravariant: Boolean, val genericModifier: GenericModifier)
  extends GenericModifier(genericModifier.ctx, genericName, upperBound, lowerBound,
    covariant, contravariant) {

  override def methods: Seq[Method] = appliedType.methods

  override def fields: Seq[Field] = appliedType.fields

  override def pkg: String = appliedType.pkg

  override def simpleName: String = appliedType.simpleName

  override def hasParent(tpe: TType): Boolean = appliedType.hasParent(tpe)

  override def isAssignableFrom(other: TType): Boolean = getConcreteType match {
    case g: GenericModifier => g.isAssignableFrom(other)
    case _@t => other.hasParent(t)
  }

  override def findField(execCtx: TType, name: String) = appliedType.findField(execCtx, name)

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]) = appliedType.findMethod(execCtx, name, args)

  override private[symbol] def parents: Seq[TType] = appliedType.parents

  override def applyType(appliedType: TType): AppliedGenericModifier = {
    throw new RuntimeException("AHHH")
  }

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case a: AppliedGenericModifier => checkVariances(a)
    case g: GenericModifier => this == g
    case c: ConcreteType => appliedType == c
    case _ => false
  }

  override def equals(that: Any): Boolean = that match {
    case a: AppliedGenericModifier => a.appliedType == appliedType
    case g: GenericModifier => appliedType == g
    case c: ConcreteType => appliedType == c
    case _ => false
  }

  def getConcreteType: TType = {
    appliedType match {
      case a: AppliedGenericModifier => a.getConcreteType
      case _ => appliedType
    }
  }

  override def isGenericModifier: Boolean = getConcreteType match {
    case g: GenericModifier => true
    case _ => false
  }

  override def gmCovariance: Boolean = genericModifier.covariance

  override def gmContravariance: Boolean = genericModifier.contravariance
}
