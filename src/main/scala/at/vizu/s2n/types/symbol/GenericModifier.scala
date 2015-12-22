package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class GenericModifier(private val _ctx: Context, val genericName: String,
                      val upperBound: TType, val lowerBound: TType,
                      val covariance: Boolean, val contravariance: Boolean) extends TType {

  override def ctx: Context = _ctx

  override def isObject: Boolean = false

  override def methods: Seq[Method] = Seq()

  override def pkg: String = ""

  override def simpleName: String = genericName

  override def hasParent(tpe: TType): Boolean = upperBound.hasParent(tpe)

  override def isAssignableFrom(other: TType): Boolean = other.hasParent(upperBound)

  override def fields: Seq[Field] = Seq()

  override def validate(): Unit = return

  override def findField(execCtx: TType, name: String) = upperBound.findField(execCtx, name)

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]) = upperBound.findMethod(execCtx, name, args)

  override def mods: Seq[Modifier] = Seq()

  override private[symbol] def parents: Seq[TType] = Seq(upperBound)

  def gmCovariance = covariance

  def gmContravariance = contravariance

  def isGenericModifier = true

  def applyType(appliedType: TType) = appliedType match {
    case g: GenericModifier => new AppliedGenericModifier(g, g.genericName, g.upperBound, g.lowerBound, g.covariance, g.contravariance, this)
    case _@o => new AppliedGenericModifier(o, genericName, upperBound, lowerBound, false, false, this) // we dont need variances in concrete apply
  }

  def checkBeforeApply(appliedType: TType): Unit = appliedType match {
    case a: AppliedGenericModifier =>
      a.appliedType match {
        case g: GenericModifier => checkBeforeApply(g)
        case _ =>
      }
    case g: GenericModifier => checkBeforeApply(g)
    case _ =>
  }

  def checkBeforeApply(genericModifier: GenericModifier) = {
    if (!checkSameVariance(genericModifier)) {
      throw new RuntimeException("Variances")
    }
  }

  override def equals(that: Any) = {
    that match {
      case g: GenericModifier =>
        g.genericName == genericName &&
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
    case a: AppliedGenericModifier =>
      a.genericModifier == this &&
        a.appliedType.isAssignableFrom(lowerBound) && upperBound.isAssignableFrom(a.appliedType)
    case g: GenericModifier => this == g
    case g: GenericType => lowerBound.hasParent(g) && upperBound.isAssignableFrom(g)
    case c: ConcreteType => c.isAssignableFrom(lowerBound) && upperBound.isAssignableFrom(c)
    case _ => false
  }

  def checkVariances(other: GenericModifier) = {
    if (gmCovariance) {
      checkCovariance(other)
    } else if (gmContravariance) {
      checkContravariance(other)
    } else {
      this == other
    }
  }

  def checkCovariance(other: GenericModifier): Boolean = {
    hasParent(other)
  }

  def checkContravariance(other: GenericModifier): Boolean = {
    other.hasParent(this)
  }

  def checkSameVariance(other: GenericModifier): Boolean = {
    this.covariance == other.covariance && this.contravariance == other.contravariance
  }
}
