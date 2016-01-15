package at.vizu.s2n.lib

import at.vizu.s2n.gen.proto.Meta._
import at.vizu.s2n.types.symbol._

import scala.collection.mutable

/**
  * Phil on 04.01.16.
  */
class ProtoBufClassMetaInfoDeserializer(scope: TScope) extends ClassMetaInfoDeserializer {

  val types = mutable.Map[String, TType]()
  val metaGenericModifier = mutable.ArrayBuffer[MetaGenericModifier]()

  override def deserialize(bytes: Array[Byte]): Seq[TType] = {
    val metaInfo: MetaInfo = MetaInfo.parseFrom(bytes)
    gatherTypes(metaInfo)
    fillGenericModifier(metaInfo)
    gatherAppliedTypes(metaInfo)
    fillTypes(metaInfo)
    types.values.filter {
      case a: AppliedGenericType => false
      case c: ConcreteType => true
      case _ => false
    }.toSeq
  }

  private def gatherTypes(metaInfo: MetaInfo) = {
    metaInfo.concreteTypes.foreach(c => types += (getTypeNameMeta(c) -> concreteTypeShell(c)))
    metaInfo.genericTypes.foreach(g => types += (getTypeNameMeta(g) -> genericTypeShell(g)))
    metaGenericModifier.foreach(gm => types += (gm.serializationId -> metaGenericModifierToReal(gm)))
  }

  private def fillGenericModifier(metaInfo: MetaInfo) = {
    metaInfo.genericTypes.foreach(meta => {
      val tpe = getGenericType(getTypeNameMeta(meta))
      meta.genericModifiers.map(g => getGenericModifier(g.serializationId)).foreach(g => tpe.addGenericModifier(g))
    })
  }

  private def gatherAppliedTypes(metaInfo: MetaInfo) = {
    metaInfo.appliedGenericModifier.foreach(gm => types += (gm.serializationId -> metaAppliedGenericModifierToReal(gm)))
    metaInfo.appliedGenericTypes.foreach(gt => types += (gt.serializationId -> metaAppliedGenericTypeToReal(gt)))
  }

  private def concreteTypeShell(meta: MetaConcreteType) = {
    val ctx: Context = metaContextToReal(meta.ctx)
    val mods = meta.mods.map(metaModToReal)
    val pkg = meta.pkg.getOrElse("")
    metaGenericModifier ++= meta.methods.flatMap(_.genericModifiers)
    ConcreteType(ctx, meta.simpleName, pkg, mods, meta.isObject.isDefined)
  }

  private def genericTypeShell(meta: MetaGenericType): GenericType = {
    val ctx = metaContextToReal(meta.ctx)
    val mods = meta.mods.map(metaModToReal)
    val pkg = meta.pkg.getOrElse("")
    metaGenericModifier ++= meta.methods.flatMap(_.genericModifiers)
    metaGenericModifier ++= meta.genericModifiers
    new GenericType(ctx, meta.simpleName, pkg, mods, meta.isObject.isDefined)
  }

  private def metaGenericModifierToReal(meta: MetaGenericModifier) = {
    val ctx = metaContextToReal(meta.ctx)
    val name: String = meta.genericName
    val upperBound = meta.upperBoundTypeId.map(findType).getOrElse(TypeUtils.anyType(scope))
    val lowerBound = meta.lowerBoundTypeId.map(findType).getOrElse(TypeUtils.nothingType(scope))
    new GenericModifier(ctx, name, upperBound, lowerBound, meta.covariant.isDefined, meta.contravariant.isDefined)
  }

  private def metaAppliedGenericModifierToReal(meta: MetaAppliedGenericModifier) = {
    val at = findType(getTypeNameMeta(meta.appliedTypeName))
    val gm = getGenericModifier(meta.genericModifier)
    at match {
      case g: GenericModifier => new AppliedGenericModifier(g, g.genericName, g.upperBound, g.lowerBound, g.covariance, g.contravariance, gm)
      case _@o => new AppliedGenericModifier(o, gm.genericName, gm.upperBound, gm.lowerBound, false, false, gm)
    }
  }

  private def metaAppliedGenericTypeToReal(meta: MetaAppliedGenericType) = {
    val genericType = getGenericType(meta.genericType)
    val appliedTypes = meta.appliedTypes.map(findType)
    genericType.applyTypeSeq(appliedTypes)
  }

  private def metaContextToReal(metaContext: MetaContext) = {
    Context(metaContext.fileName, metaContext.line)
  }

  private def metaModToReal(metaMod: MetaModifier) = {
    metaMod match {
      case MetaModifier.ABSTRACT => Abstract
      case MetaModifier.CASE => Case
      case MetaModifier.FINAL => Final
      case MetaModifier.MUTABLE => Mutable
      case MetaModifier.OVERRIDE => Override
      case MetaModifier.PACKAGEPRIVATE => PackagePrivate
      case MetaModifier.PARAM_ACCESSOR => ParamAccessor
      case MetaModifier.PRIVATE => Private
      case MetaModifier.PROTECTED => Protected
      case MetaModifier.SEALED => Sealed
      case MetaModifier.TRAIT => Trait
    }
  }

  private def fillTypes(metaInfo: MetaInfo) = {
    metaInfo.concreteTypes.foreach(fillConcreteType)
    metaInfo.genericTypes.foreach(fillGenericType)
  }

  private def fillConcreteType(meta: MetaConcreteType): Unit = {
    val tpe = getConcreteType(getTypeNameMeta(meta))
    fillConcreteType(tpe, meta.fields, meta.methods, meta.parents)
  }

  private def fillGenericType(meta: MetaGenericType) = {
    val tpe = getGenericType(getTypeNameMeta(meta))
    fillConcreteType(tpe, meta.fields, meta.methods, meta.parents)
    //meta.genericModifiers.map(g => getGenericModifier(g.serializationId)).foreach(g => tpe.addGenericModifier(g))
  }

  private def fillConcreteType(ct: ConcreteType, fs: Seq[MetaField], ms: Seq[MetaMethod], ps: Seq[String]): Unit = {
    val fields = fs.map(metaFieldToReal)
    val methods = ms.map(metaMethodToReal)
    val parents = ps.map(p => Parent(findType(p)))
    fields.foreach(f => ct.addField(f))
    methods.foreach(m => ct.addMethod(m))
    parents.foreach(p => ct.addParent(p))
  }

  private def metaFieldToReal(meta: MetaField): Field = {
    val ctx = metaContextToReal(meta.ctx)
    val mods = meta.mods.map(metaModToReal)
    val tpe = findType(meta.typeName)
    Field(ctx, mods, meta.name, tpe)
  }

  private def metaMethodToReal(meta: MetaMethod): Method = {
    val ctx = metaContextToReal(meta.ctx)
    val tpe = findType(meta.typeName)
    val mods = meta.mods.map(metaModToReal)
    val params = meta.params.map(metaParamToReal)
    val generics = meta.genericModifiers.map(g => getGenericModifier(g.serializationId))
    val constructor: Boolean = meta.constructor.isDefined
    val instanceMethod: Boolean = meta.instanceMethod.isDefined
    val operator: Boolean = meta.operator.isDefined
    Method(ctx, meta.name, tpe, mods, params, generics, constructor, instanceMethod, operator)
  }

  private def metaParamToReal(meta: MetaParam): Param = {
    val ctx = metaContextToReal(meta.ctx)
    val tpe = findType(meta.typeName)
    Param(ctx, tpe, meta.name, meta.hasDefaultVal.isDefined, meta.mutable.isDefined)
  }

  private def getGenericModifier(id: String): GenericModifier = {
    findType(id).asInstanceOf[GenericModifier]
  }

  private def getGenericType(id: String): GenericType = {
    findType(id).asInstanceOf[GenericType]
  }

  private def getConcreteType(id: String): ConcreteType = {
    findType(id).asInstanceOf[ConcreteType]
  }

  private def findType(id: String) = {
    types.get(id) orElse scope.findClass(id) orElse
      scope.findObject(id) getOrElse (throw new RuntimeException("Halp: " + id))
  }

  private def getTypeName(tpe: TType) = {
    tpe match {
      case gm: GenericModifier => gm.serializationId
      case at: AppliedGenericType => at.serializationId
      case _@t => t.fullClassName
    }
  }

  private def getTypeNameMeta(tpe: Any): String = {
    tpe match {
      case c: MetaConcreteType => c.pkg.map(_ + ".").getOrElse("") + c.simpleName
      case g: MetaGenericType => g.pkg.map(_ + ".").getOrElse("") + g.simpleName
      case a: MetaAppliedGenericType => a.serializationId
      case g: MetaGenericModifier => g.serializationId
      case a: AppliedGenericModifier => a.serializationId
    }
  }
}
