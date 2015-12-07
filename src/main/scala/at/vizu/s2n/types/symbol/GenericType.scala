package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class GenericType(_ctx: Context = Context("", 0), _simpleName: String,
                  _pkg: String = "", _mods: Seq[Modifier] = Vector(), genericModifiers: Seq[GenericModifier]
                  , override private[symbol] val _isObject: Boolean = false)
  extends ConcreteType(_ctx, _simpleName, _pkg, _mods, _isObject) {

  def applyTypes(types: Seq[TType]) = {
    val typeMap: Map[GenericModifier, TType] = genericModifiers.zip(types).toMap
    val newMethods = mapMethods(typeMap)
    val newFields = mapFields(typeMap)
    new AppliedGenericType(types, newMethods, newFields, this)
  }

  private def mapMethods(types: Map[GenericModifier, TType]): Seq[Method] = {
    methods.map(mapMethod(types, _))
  }

  private def mapMethod(types: Map[GenericModifier, TType], method: Method) = {
    val returnType = getNewTpe(types, method.returnType)
    val params = method.params.map(mapParam(types, _))
    Method(method.ctx, method.name, returnType, method.mods, params, method.constructor, method.instanceMethod, method.operator)
  }

  private def mapParam(types: Map[GenericModifier, TType], oldParam: Param): Param = {
    oldParam.tpe match {
      case g: GenericModifier => Param(oldParam.ctx, getNewTpe(types, g), oldParam.name, oldParam.hasDefaultVal, oldParam.mutable)
      case _ => oldParam // Garbage collector?
    }
  }

  private def mapFields(types: Map[GenericModifier, TType]): Seq[Field] = {
    fields.map(mapField(types, _))
  }

  private def mapField(types: Map[GenericModifier, TType], field: Field) = {
    field.tpe match {
      case g: GenericModifier => Field(field.ctx, field.mods, field.name, getNewTpe(types, g))
      case _ => field // Garbage collector?
    }
  }

  private def getNewTpe(types: Map[GenericModifier, TType], oldType: TType) = {
    oldType match {
      case g: GenericModifier => types.get(g).get
      case _ => oldType
    }
  }
}
