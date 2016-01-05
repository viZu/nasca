package at.vizu.s2n.types.symbol

import at.vizu.s2n.id.IdGenerator

/**
  * Phil on 07.12.15.
  */
class AppliedGenericType(val appliedTypes: Seq[GenericModifier],
                         val genericType: GenericType) extends GenericType(
  genericType.ctx, genericType.simpleName, genericType.pkg, genericType.mods, genericType.isObject) {

  lazy val serializationId: String = IdGenerator.generateId()

  genericType.memberAddedListener += memberAddedInGenericType

  private val appliedTypeMap = genericType.genericModifiers.zip(appliedTypes).toMap

  private def memberAddedInGenericType(member: Member) = {
    member match {
      case f: Field => addField(mapField(appliedTypeMap, f, this))
      case m: Method => addMethod(mapMethod(appliedTypeMap, m, this))
    }
  }

  override def genericModifiers: Seq[GenericModifier] = genericType.genericModifiers

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
    case a: AppliedGenericType => a.genericType == genericType
    case b: GenericType => genericType == b
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
}
