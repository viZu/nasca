package at.vizu.s2n.lib

import at.vizu.s2n.gen.proto.Meta._
import at.vizu.s2n.types.symbol.TypeUtils._
import at.vizu.s2n.types.symbol._

import scala.collection.mutable

/**
  * Phil on 04.01.16.
  */
class ProtoBufClassMetaInfoSerializer extends ClassMetaInfoSerializer {

  private val appliedGenericModifiers = mutable.Set[AppliedGenericModifier]()
  private val appliedGenericTypes = mutable.Set[AppliedGenericType]()
  private val anyType = RootScalaPackage + ".Any"
  private val nothingType = RootScalaPackage + ".Nothing"

  override def serialize(classInfos: Seq[TType]): Array[Byte] = {
    val metaInfo: MetaInfo = typesToMetaInfo(classInfos)
    metaInfo.toByteArray
  }

  private def typesToMetaInfo(types: Seq[TType]): MetaInfo = {
    val concreteMetaInfo = types.collect({
      case g: GenericType => None
      case c: ConcreteType => Some(c)
    }).flatten.map(concreteTypeToMeta)
    val genericMetaInfo = types.collect { case g: GenericType => g }.map(genericTypeToMeta)
    val appliedGenericModifierMetas = this.appliedGenericModifiers.map(appliedGenericModifierToMeta).toSeq
    val appliedGenericTypeMetas = this.appliedGenericTypes.map(appliedGenericTypeToMeta).toSeq
    MetaInfo(concreteMetaInfo, genericMetaInfo, appliedGenericTypeMetas, appliedGenericModifierMetas)
  }

  private def concreteTypeToMeta(tpe: ConcreteType): MetaConcreteType = {
    val metaCtx = contextToMeta(tpe.ctx)
    val pkg = if (tpe.pkg.isEmpty) None else Some(tpe.pkg)
    val modifier = tpe.mods.map(modifierToMeta)
    val isObject: Option[Boolean] = boolToOption(tpe.isObject)
    val methods = tpe.methods.map(methodToMeta)
    val fields = tpe.fields.map(fieldToMeta)
    val parents = tpe.parents.map(p => handleType(p.tpe))
    MetaConcreteType(metaCtx, tpe.simpleName, pkg, modifier, isObject, methods, fields, parents)
  }

  private def genericTypeToMeta(tpe: GenericType): MetaGenericType = {
    val metaCtx = contextToMeta(tpe.ctx)
    val pkg = if (tpe.pkg.isEmpty) None else Some(tpe.pkg)
    val modifier = tpe.mods.map(modifierToMeta)
    val isObject: Option[Boolean] = boolToOption(tpe.isObject)
    val genericMods = tpe.genericModifiers.map(genericModToMeta)
    val methods = tpe.methods.map(methodToMeta)
    val fields = tpe.fields.map(fieldToMeta)
    val parents = tpe.parents.map(p => handleType(p.tpe))
    MetaGenericType(metaCtx, tpe.simpleName, pkg, modifier, isObject, genericMods, methods, fields, parents)
  }

  private def appliedGenericTypeToMeta(tpe: AppliedGenericType): MetaAppliedGenericType = {
    val id = tpe.serializationId
    val genTypeId = handleType(tpe.genericType)
    val appliedTypes = tpe.appliedTypes.map(handleType)
    MetaAppliedGenericType(id, genTypeId, appliedTypes)
  }

  private def appliedGenericModifierToMeta(agm: AppliedGenericModifier): MetaAppliedGenericModifier = {
    val metaCtx = contextToMeta(agm.ctx)
    val appliedType = handleType(agm.appliedType)
    val genericModifier = agm.genericModifier.serializationId
    MetaAppliedGenericModifier(agm.serializationId, metaCtx, appliedType, genericModifier)
  }

  def methodToMeta(method: Method): MetaMethod = {
    val metaCtx = contextToMeta(method.ctx)
    val typeName = handleType(method.tpe)
    val modifier = method.modifiers.map(modifierToMeta).toSeq
    val params = method.params.map(paramToMeta)
    val genericMods = method.generics.map(genericModToMeta)
    val constructor = boolToOption(method.constructor)
    val instanceMethod = boolToOption(method.instanceMethod)
    val operator = boolToOption(method.operator)
    MetaMethod(metaCtx, method.name, typeName, modifier, params, genericMods, constructor, instanceMethod, operator)
  }

  def genericModToMeta(gm: GenericModifier): MetaGenericModifier = {
    val metaCtx = contextToMeta(gm.ctx)
    val ub = if (gm.upperBound.fullClassName == anyType) None else Some(handleType(gm.upperBound))
    val lb = if (gm.lowerBound.fullClassName == nothingType) None else Some(handleType(gm.lowerBound))
    val covariant = boolToOption(gm.covariance)
    val contravariant = boolToOption(gm.contravariance)
    MetaGenericModifier(gm.serializationId, metaCtx, gm.name, ub, lb, covariant, contravariant)
  }

  def paramToMeta(param: Param): MetaParam = {
    val metaCtx = contextToMeta(param.ctx)
    val typeName = handleType(param.tpe)
    val defaultValue = boolToOption(param.hasDefaultVal)
    val mutable = boolToOption(param.mutable)
    MetaParam(metaCtx, typeName, param.name, defaultValue, mutable)
  }

  def fieldToMeta(field: Field): MetaField = {
    val metaCtx = contextToMeta(field.ctx)
    val mods = field.mods.map(modifierToMeta)
    MetaField(metaCtx, mods, field.name, handleType(field.tpe))
  }

  private def contextToMeta(ctx: Context): MetaContext = {
    MetaContext(ctx.fileName, ctx.line)
  }

  private def modifierToMeta(mod: Modifier): MetaModifier = {
    mod match {
      case Private => MetaModifier.PRIVATE
      case PackagePrivate => MetaModifier.PACKAGEPRIVATE
      case Protected => MetaModifier.PROTECTED
      case ParamAccessor => MetaModifier.PARAM_ACCESSOR
      case Trait => MetaModifier.TRAIT
      case Abstract => MetaModifier.ABSTRACT
      case Override => MetaModifier.OVERRIDE
      case Sealed => MetaModifier.SEALED
      case Final => MetaModifier.FINAL
      case Case => MetaModifier.CASE
      case Mutable => MetaModifier.MUTABLE
    }
  }

  private def boolToOption(bool: Boolean): Option[Boolean] = {
    bool match {
      case true => Some(true)
      case _ => None
    }
  }

  private def handleType(tpe: TType): String = {
    tpe match {
      case am: AppliedGenericModifier =>
        appliedGenericModifiers += am
        handleType(am.getConcreteType)
      case gm: GenericModifier =>
        gm.serializationId
      case at: AppliedGenericType =>
        appliedGenericTypes += at
        at.serializationId
      case _@t => t.fullClassName
    }
  }
}
