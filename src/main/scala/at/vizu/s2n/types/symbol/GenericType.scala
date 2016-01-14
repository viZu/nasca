package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class GenericType(_ctx: Context = Context("", 0), _simpleName: String,
                  _pkg: String = "", _mods: Seq[Modifier] = Vector(),
                  override private[symbol] val _isObject: Boolean = false)
  extends ConcreteType(_ctx, _simpleName, _pkg, _mods, _isObject) {

  private var _genericModifiers: Seq[GenericModifier] = Vector()

  def genericModifiers: Seq[GenericModifier] = _genericModifiers

  def addGenericModifier(genericModifier: GenericModifier) = _genericModifiers = _genericModifiers :+ genericModifier

  def applyTypeSeq(types: Seq[TType]): AppliedGenericType = applyTypes(_genericModifiers.zip(types).toMap)

  def applyTypes(typeMap: Map[GenericModifier, TType]): AppliedGenericType = {
    this match {
      case a: AppliedGenericType if !typeMap.values.exists(_ != null) => a
      case _ =>
        val appliedType = new AppliedGenericType(getAppliedTypes(typeMap), this)
        val newMethods = mapMethods(typeMap, appliedType)
        val newFields = mapFields(typeMap, appliedType)
        val newParents = mapParents(typeMap)
        newMethods.foreach(appliedType.addMethod)
        newFields.foreach(appliedType.addField)
        newParents.foreach(appliedType.addParent)
        appliedType
    }
  }

  protected def mapParents(types: Map[GenericModifier, TType]): Seq[Parent] = {
    parentTypes.map {
      case g: GenericType => Parent(g.applyTypes(types))
      case _@p => Parent(p)
    }
  }

  protected def mapMethods(types: Map[GenericModifier, TType], appliedType: TType): Seq[Method] = {
    methods.map(mapMethod(types, _, appliedType))
  }

  protected def mapMethod(types: Map[GenericModifier, TType], method: Method, appliedType: TType) = {
    val returnType = getNewTpe(types, method.returnType, appliedType, applyPartly = true)
    val params = method.params.map(mapParam(types, _, appliedType))
    Method(method.ctx, method.name, returnType, method.mods, params, method.generics,
      method.constructor, method.instanceMethod, method.operator)
  }

  protected def mapParam(types: Map[GenericModifier, TType], oldParam: Param, appliedType: TType): Param = {
    //TODO check if method has generic modifier
    oldParam.tpe match {
      case g: GenericModifier => Param(oldParam.ctx, getNewTpe(types, g, appliedType, applyPartly = false),
        oldParam.name, oldParam.hasDefaultVal, oldParam.mutable)
      case _ => oldParam // Garbage collector?
    }
  }

  protected def mapFields(types: Map[GenericModifier, TType], appliedType: TType): Seq[Field] = {
    fields.map(mapField(types, _, appliedType))
  }

  protected def mapField(types: Map[GenericModifier, TType], field: Field, appliedType: TType) = {
    field.tpe match {
      case g: GenericModifier => Field(field.ctx, field.mods, field.name,
        getNewTpe(types, g, appliedType, applyPartly = false))
      case _ => field // Garbage collector?
    }
  }

  private def getAppliedTypes(appliedTypes: Map[GenericModifier, TType]): Seq[GenericModifier] = {
    getGenericModifiers.map(gm => {
      if (appliedTypes.get(gm).get == null) gm
      else gm.applyType(appliedTypes.get(gm).get)
    })
  }

  private def getNewTpe(types: Map[GenericModifier, TType], oldType: TType, appliedType: TType, applyPartly: Boolean = false) = {
    oldType match {
      case am: AppliedGenericModifier =>
        am.getConcreteType match {
          case g: GenericModifier => types.get(g) match {
            case None => g
            case Some(t) => g.applyType(t)
          }
          case _ => ???
        }
      case g: GenericModifier => types.get(g) match {
        case None => g
        case Some(t) => g.applyType(t)
      }
      case g: GenericType if this == g => appliedType
      case g: GenericType =>
        val typesToApply = if (applyPartly) findTypesToApplyPartly(types, g) else findTypesToApply(types, g)
        g.applyTypes(typesToApply) // TODO apply partly
      case _ => oldType
    }
  }

  private def findTypesToApply(types: Map[GenericModifier, TType], g: GenericType): Map[GenericModifier, TType] = {
    g.getGenericModifiers.map(gm => (gm, types.getOrElse(gm, gm.upperBound))).toMap
  }

  private def findTypesToApplyPartly(types: Map[GenericModifier, TType], g: GenericType): Map[GenericModifier, TType] = {
    g.getGenericModifiers.map(gm => (gm, types.getOrElse(gm, null))).toMap
  }

  override def toString: String = s"$name[${TypeUtils.toString(_genericModifiers)}]"

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AppliedGenericType =>
      this == a.genericType && genericModifiers.zip(a.appliedTypes).forall(t => t._1.typeEquals(t._2))
    case g: GenericType => this.eq(g)
    case _ => false
  }

  def baseTypeEquals(obj: TType): Boolean = {
    obj match {
      case a: AppliedGenericType => this == a.genericType
      case b: GenericType => this == b
      case _ => false
    }
  }

  override def hasParent(tpe: TType): Boolean = super.hasParent(tpe)

  override def isAssignableAsParam(other: TType): Boolean = other match {
    case a: AppliedGenericType =>
      //      a.genericType == this &&
      //        genericModifiers.zip(a.appliedTypes).forall({ case (gm, tpe) => gm.isAssignableFrom(tpe) })
      false // is not possible
    case g: GenericType => this == g
    case _ => false
  }

  def getGenericModifiers: Seq[GenericModifier] = {
    genericModifiers
  }
}
