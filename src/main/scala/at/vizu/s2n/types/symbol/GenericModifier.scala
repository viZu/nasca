package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class GenericModifier(private val _ctx: Context, val genericName: String,
                      val upperBound: TType, val lowerBound: TType,
                      val coVariance: Boolean, val contraVariance: Boolean) extends TType {

  override def ctx: Context = _ctx

  override def isObject: Boolean = false

  override def methods: Seq[Method] = Seq()

  override def pkg: String = ""

  override def simpleName: String = genericName

  override def hasParent(tpe: TType): Boolean = upperBound.hasParent(tpe)

  override def fields: Seq[Field] = Seq()

  override def validate(): Unit = return

  override def findField(execCtx: TType, name: String) = upperBound.findField(execCtx, name)

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]) = upperBound.findMethod(execCtx, name, args)

  override def mods: Seq[Modifier] = Seq()

  override private[symbol] def parents: Seq[TType] = Seq(upperBound)

  def applyType(appliedType: TType) = new AppliedGenericModifier(appliedType, this)

  override def equals(that: Any) = {
    that match {
      case g: GenericModifier =>
        g.genericName == genericName &&
          g.upperBound == upperBound &&
          g.lowerBound == lowerBound &&
          g.coVariance == coVariance &&
          g.contraVariance == contraVariance
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + genericName.hashCode
    result = prime * result + upperBound.hashCode()
    result = prime * result + lowerBound.hashCode()
    result = prime * result + coVariance.hashCode()
    result = prime * result + contraVariance.hashCode()
    result
  }
}
