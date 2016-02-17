package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class GenericType(_ctx: Context = Context("", 0), _simpleName: String,
                  _pkg: String = "", _mods: Seq[Modifier] = Vector(),
                  override private[symbol] val _isObject: Boolean = false)
  extends ConcreteType(_ctx, _simpleName, _pkg, _mods, _isObject) {

  val genericType: GenericType = this
  private var _genericModifiers: Seq[TypeArgument] = Vector()

  def genericModifiers: Seq[TypeArgument] = _genericModifiers

  def addGenericModifier(genericModifier: TypeArgument) = _genericModifiers = _genericModifiers :+ genericModifier

  def applyTypeSeq(scope: TScope, types: Seq[TType]): AppliedGenericType = {
    applyTypes(scope, _genericModifiers.zip(types).toMap)
  }

  def applyTypes(scope: TScope, typeMap: Map[TypeArgument, TType]): AppliedGenericType = {
    scope.findAppliedType(typeMap, this) match {
      case Some(found) => found
      case None =>
        this match {
          case a: AppliedGenericType if !typeMap.values.exists(_ != null) => a
          case _ =>
            val appliedType = new AppliedGenericType(getAppliedTypes(typeMap), this)
            scope.addAppliedType(typeMap, this, appliedType)
            val newMethods = mapMethods(scope, typeMap, appliedType)
            val newFields = mapFields(scope, typeMap, appliedType)
            val newParents = mapParents(scope, typeMap)
            newMethods.foreach(appliedType.addMethod)
            newFields.foreach(appliedType.addField)
            newParents.foreach(appliedType.addParent)
            appliedType
        }
    }
  }

  protected def mapParents(scope: TScope, types: Map[TypeArgument, TType]): Seq[Parent] = {
    parentTypes.map {
      case g: GenericType => Parent(g.applyTypes(scope, types))
      case _@p => Parent(p)
    }
  }

  protected def mapMethods(scope: TScope, types: Map[TypeArgument, TType], appliedType: TType): Seq[Method] = {
    methods.map(mapMethod(scope, types, _, appliedType))
  }

  protected def mapMethod(scope: TScope, types: Map[TypeArgument, TType], method: Method, appliedType: TType) = {
    val returnType = getNewTpe(scope, types, method.returnType, appliedType, applyPartly = true)
    val params = method.params.map(mapParam(scope, types, _, appliedType))
    method match {
      case c: Constructor => Constructor(c.ctx, returnType, c.mods, params, c.primary)
      case m: Method => Method(method.ctx, method.name, returnType, method.mods, params, method.generics,
        method.instanceMethod, method.operator, method.nonPointer)
    }

  }

  protected def mapReturnType(scope: TScope, types: Map[TypeArgument, TType], returnType: TType, appliedType: TType) = {
    if (this == returnType) appliedType
    else getNewTpe(scope, types, returnType, appliedType, applyPartly = true)
  }

  protected def mapParam(scope: TScope, types: Map[TypeArgument, TType], oldParam: Param, appliedType: TType): Param = {
    //TODO check if method has generic modifier
    oldParam.tpe match {
      case g: TypeArgument => Param(oldParam.ctx, getNewTpe(scope, types, g, appliedType, applyPartly = true),
        oldParam.name, oldParam.hasDefaultVal, oldParam.mutable)
      case g: GenericType if g.genericModifiers.nonEmpty =>
        val newTpe: TType = getNewTpe(scope, types, g, appliedType, applyPartly = true)
        Param(oldParam.ctx, newTpe, oldParam.name, oldParam.hasDefaultVal, oldParam.mutable)
      case _ => oldParam // Garbage collector?
    }
  }

  protected def mapFields(scope: TScope, types: Map[TypeArgument, TType], appliedType: TType): Seq[Field] = {
    fields.map(mapField(scope, types, _, appliedType))
  }

  protected def mapField(scope: TScope, types: Map[TypeArgument, TType], field: Field, appliedType: TType) = {
    field.tpe match {
      case g: TypeArgument => Field(field.ctx, field.mods, field.name,
        getNewTpe(scope, types, g, appliedType, applyPartly = false))
      case g: GenericType =>
        val newTpe: TType = getNewTpe(scope, types, g, appliedType, applyPartly = false)
        Field(field.ctx, field.mods, field.name, newTpe)
      case _ => field // Garbage collector?
    }
  }

  protected def getAppliedTypes(appliedTypes: Map[TypeArgument, TType]): Seq[TypeArgument] = {
    getGenericModifiers.map(gm => {
      if (appliedTypes.get(gm).get == null) gm
      else gm.applyType(appliedTypes.get(gm).get)
    })
  }

  private def getNewTpe(scope: TScope, types: Map[TypeArgument, TType], oldType: TType,
                        appliedType: TType, applyPartly: Boolean = false) = {
    oldType match {
      case am: AppliedTypeArgument =>
        am.getConcreteType match {
          case g: TypeArgument => types.get(g) match {
            case None => g
            case Some(t) => g.applyType(t)
          }
          case _@t => t
        }
      case g: TypeArgument =>
        types.get(g) match {
          case None => g
          case Some(t) => g.applyType(t)
        }
      case g: GenericType if this == g =>
        appliedType
      case g: GenericType =>
        val typesToApply = if (applyPartly) findTypesToApplyPartly(types, g) else findTypesToApply(types, g)
        g.applyTypes(scope, typesToApply) // TODO apply partly
      case _ => oldType
    }
  }

  private def findTypesToApply(types: Map[TypeArgument, TType], g: GenericType): Map[TypeArgument, TType] = {
    g.getGenericModifiers.map(gm => (gm, types.getOrElse(gm, gm.upperBound))).toMap
  }

  private def findTypesToApplyPartly(types: Map[TypeArgument, TType], g: GenericType): Map[TypeArgument, TType] = {
    g.getGenericModifiers.map(gm => (gm, types.getOrElse(gm, null))).toMap
  }

  override def toString: String = s"$name[${TypeUtils.toString(_genericModifiers)}]"

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AppliedGenericType =>
      this == a.genericType && genericModifiers == a.appliedTypes
    case g: GenericType => this.eq(g)
    case _ => false
  }

  override def typeEquals(obj: Any): Boolean = obj match {
    case a: AppliedGenericType =>
      this == a.genericType && genericModifiers.zip(a.appliedTypes).forall(t => t._1.typeEquals(t._2))
    case g: GenericType => this.eq(g)
    case _ => false
  }

  override def baseTypeEquals(obj: TType): Boolean = {
    obj match {
      case a: AppliedTypeArgument => baseTypeEquals(a.getConcreteType)
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

  def getGenericModifiers: Seq[TypeArgument] = {
    genericModifiers
  }
}
