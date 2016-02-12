package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class AppliedTypeArgument(val appliedType: TType, genericName: String, upperBound: TType, lowerBound: TType,
                          covariant: Boolean, contravariant: Boolean, val genericModifier: TypeArgument)
  extends TypeArgument(genericModifier.ctx, genericName, upperBound, lowerBound,
    covariant, contravariant) {

  override def methods: Seq[Method] = appliedType.methods

  override def fields: Seq[Field] = appliedType.fields

  override def pkg: String = appliedType.pkg

  override def simpleName: String = appliedType.simpleName

  override def hasParent(tpe: TType): Boolean = appliedType.hasParent(tpe)

  override def isAssignableFrom(other: TType): Boolean = getConcreteType match {
    case g: TypeArgument => g.isAssignableFrom(other)
    case _@t => other.hasParent(t)
  }

  override def findField(execCtx: TType, name: String) = appliedType.findField(execCtx, name)

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]) = appliedType.findMethod(execCtx, name, args)

  override def parents = appliedType.parents

  override def applyType(appliedType: TType): AppliedTypeArgument = {
    throw new RuntimeException("AHHH")
  }

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case a: AppliedTypeArgument => checkVariances(a)
    case g: TypeArgument => this == g
    case c: ConcreteType => appliedType == c
    case _ => false
  }

  override def equals(that: Any): Boolean = that match {
    case a: AppliedTypeArgument => a.getConcreteType == getConcreteType
    case g: TypeArgument => appliedType == g
    case c: ConcreteType => getConcreteType == c
    case _ => false
  }


  override def typeEquals(that: Any): Boolean = {
    that match {
      case a: AppliedTypeArgument => getConcreteType == a.getConcreteType
      case g: TypeArgument => getConcreteType == g
      case c: ConcreteType => getConcreteType == c
      case _ => false
    }
  }

  def getConcreteType: TType = {
    appliedType match {
      case a: AppliedTypeArgument => a.getConcreteType
      case _ => appliedType
    }
  }

  override def isGenericModifier: Boolean = getConcreteType match {
    case g: TypeArgument => true
    case _ => false
  }

  override def gmCovariance: Boolean = genericModifier.covariance

  override def gmContravariance: Boolean = genericModifier.contravariance

  override def baseTypeEquals(obj: TType): Boolean = getConcreteType.baseTypeEquals(obj)

  override def toString: String = appliedType.toString
}
