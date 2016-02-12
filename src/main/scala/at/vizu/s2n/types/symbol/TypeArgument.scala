package at.vizu.s2n.types.symbol

import at.vizu.s2n.id.IdGenerator

/**
  * Phil on 07.12.15.
  */
class TypeArgument(private val _ctx: Context, val genericName: String,
                   val upperBound: TType, val lowerBound: TType,
                   val covariance: Boolean, val contravariance: Boolean) extends TType {

  lazy val serializationId: String = IdGenerator.generateId()

  override def ctx: Context = _ctx

  override def isObject: Boolean = false

  override def methods: Seq[Method] = Seq()

  override def pkg: String = ""

  override def simpleName: String = genericName

  override def hasParent(tpe: TType): Boolean = this == tpe || upperBound.hasParent(tpe) || hasParentAsLowerType(tpe) ||
    hasLowerBoundAsParent(tpe)

  private def hasParentAsLowerType(tpe: TType): Boolean = tpe match {
    case a: AppliedTypeArgument => false
    case g: TypeArgument => hasParent(g.lowerBound)
    case _ => false
  }

  private def hasLowerBoundAsParent(tpe: TType) = tpe match {
    case a: AppliedTypeArgument => false
    case g: TypeArgument => tpe.hasParent(this.lowerBound)
    case _ => false
  }

  override def isAssignableFrom(other: TType): Boolean = other.hasParent(upperBound)

  override def fields: Seq[Field] = Seq()

  override def validate(): Unit = return

  override def findField(execCtx: TType, name: String) = upperBound.findField(execCtx, name)

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]) = upperBound.findMethod(execCtx, name, args)

  override def mods: Seq[Modifier] = Seq()

  override def parents: Seq[Parent] = Seq(Parent(upperBound))

  def gmCovariance = covariance

  def gmContravariance = contravariance

  def isGenericModifier = true

  def applyType(appliedType: TType) = appliedType match {
    case g: TypeArgument => new AppliedTypeArgument(g, g.genericName, g.upperBound, g.lowerBound, g.covariance, g.contravariance, this)
    case _@o => new AppliedTypeArgument(o, genericName, upperBound, lowerBound, false, false, this) // we dont need variances in concrete apply
  }

  def checkBeforeApply(appliedType: TType): Unit = appliedType match {
    case a: AppliedTypeArgument =>
      a.appliedType match {
        case g: TypeArgument => checkBeforeApply(g)
        case _ =>
      }
    case g: TypeArgument => checkBeforeApply(g)
    case _ =>
  }

  def checkBeforeApply(genericModifier: TypeArgument) = {
    if (!checkSameVariance(genericModifier)) {
      throw new RuntimeException("Variances")
    }
  }

  override def equals(that: Any) = {
    that match {
      case g: TypeArgument =>
        g.genericName == genericName &&
          g.upperBound == upperBound &&
          g.lowerBound == lowerBound &&
          g.covariance == covariance &&
          g.contravariance == contravariance
      case _ => false
    }
  }

  override def typeEquals(that: Any) = {
    that match {
      case g: TypeArgument =>
        g.upperBound == upperBound &&
          g.lowerBound == lowerBound &&
          g.covariance == covariance &&
          g.contravariance == contravariance
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + genericName.hashCode
    result = prime * result + upperBound.hashCode()
    result = prime * result + lowerBound.hashCode()
    result = prime * result + covariance.hashCode()
    result = prime * result + contravariance.hashCode()
    result
  }

  override def toString: String = {
    val prefix = if (covariance) "+" else if (contravariance) "-" else ""
    prefix + name
  }

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case a: AppliedTypeArgument =>
      a.genericModifier == this &&
        a.appliedType.isAssignableFrom(lowerBound) && upperBound.isAssignableFrom(a.appliedType)
    case g: TypeArgument => this == g
    case g: GenericType => lowerBound.hasParent(g) && upperBound.isAssignableFrom(g)
    case c: ConcreteType => c.isAssignableFrom(lowerBound) && upperBound.isAssignableFrom(c)
    case _ => false
  }

  def checkVariances(other: TypeArgument) = {
    if (gmCovariance) {
      checkCovariance(other)
    } else if (gmContravariance) {
      checkContravariance(other)
    } else {
      this == other
    }
  }

  def checkCovariance(other: TypeArgument): Boolean = {
    hasParent(other)
  }

  def checkContravariance(other: TypeArgument): Boolean = {
    other.hasParent(this)
  }

  def checkSameVariance(other: TypeArgument): Boolean = {
    this.covariance == other.covariance && this.contravariance == other.contravariance
  }

  override def baseTypeEquals(obj: TType): Boolean = typeEquals(obj)
}
