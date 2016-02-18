package at.vizu.s2n.types.symbol

import at.vizu.s2n.id.IdGenerator

/**
  * Phil on 07.12.15.
  */
class AppliedGenericType(val appliedTypes: Seq[TypeArgument],
                         override val genericType: GenericType) extends GenericType(
  genericType.ctx, genericType.simpleName, genericType.pkg, genericType.mods, genericType.isObject) {

  lazy val serializationId: String = IdGenerator.generateId()

  genericType.memberAddedListener += memberAddedInGenericType

  private val appliedTypeMap = genericType.genericModifiers.zip(appliedTypes).toMap

  private def memberAddedInGenericType(scope: TScope, member: Member) = {
    member match {
      case f: Field => addField(mapField(scope, appliedTypeMap, f))
      case m: Method => addMethod(mapMethod(scope, appliedTypeMap, m))
    }
  }


  override def parents: Seq[Parent] = _parents

  override def applyTypes(scope: TScope, typeMap: Map[TypeArgument, TType]): AppliedGenericType = {
    if (getGenericModifiers.isEmpty) this
    else super.applyTypes(scope, typeMap)
  }

  override def genericModifiers: Seq[TypeArgument] = genericType.genericModifiers

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
        appliedTypes.zip(a.appliedTypes).forall({ case (tat, at) => tat.isAssignableAsParam(at) })
    case g: GenericType =>
      this.genericType == g &&
        appliedTypes.zip(g.genericModifiers).forall({ case (tpe, gm) => tpe.isAssignableAsParam(gm) })
    case _ => false
  }

  override def baseTypeEquals(obj: TType): Boolean = obj match {
    case a: AppliedTypeArgument => baseTypeEquals(a.getConcreteType)
    case a: AppliedGenericType => a.getBaseType.typeEquals(getBaseType)
    case b: GenericType => getBaseType.typeEquals(b)
    case _ => false
  }

  override def hasParent(tpe: TType): Boolean = {
    if (super.hasParent(tpe)) true
    else {
      tpe match {
        case a: AppliedGenericType =>
          if (baseTypeEquals(a)) {
            appliedTypes.zip(a.appliedTypes).forall {
              case (param, arg) =>
                if (!param.isGenericModifier && !arg.isGenericModifier) {
                  param.checkVariances(arg)
                } else {
                  arg.isAssignableFrom(param)
                }
            }
          } else {
            false
          }
        case _ => false
      }
    }
  }

  override def getGenericModifiers: Seq[TypeArgument] = {
    appliedTypes.map({
      case a: AppliedTypeArgument => a.getConcreteType
    }).collect({ case g: TypeArgument => g })
  }

  override protected def getAppliedTypes(appliedTypes: Map[TypeArgument, TType]): Seq[TypeArgument] = {
    this.appliedTypes.map({
      case a: AppliedTypeArgument => a.getConcreteType match {
        case g: TypeArgument if appliedTypes.get(g).get != null => g.applyType(appliedTypes.get(g).get)
        case _@t => a
      }
    })
  }

  def getBaseType: GenericType = {
    genericType match {
      case a: AppliedGenericType => a.getBaseType
      case g: GenericType => g
    }
  }
}