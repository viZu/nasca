package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 21.10.15.
 */
object TypeUtils {

  val ConstructorName = "<init>"

  private var unitTpe: TType = null
  private var nullTpe: TType = null
  private var anyTpe: TType = null
  private var nothingTpe: TType = null

  /**
    * Modifiers
    */

  def getModifiers(mods: Modifiers): Seq[Modifier] = {
    var modifiers: Seq[Modifier] = Vector[Modifier]()

    if (isZero(mods.flags)) {
      modifiers
    } else {
      if (mods.privateWithin.toString != "") {
        //TODO which package?
        modifiers = PackagePrivate +: modifiers
      }
      if (mods.hasFlag(Flag.PRIVATE)) {
        // TODO PARAMACCESSOR -> generate Getter/Setter?
        modifiers = Private +: modifiers
      }
      if (mods.hasFlag(Flag.PARAMACCESSOR)) {
        modifiers = ParamAccessor +: modifiers
      }
      if (mods.hasFlag(Flag.PROTECTED)) {
        modifiers = Protected +: modifiers
      }
      if (mods.hasFlag(Flag.ABSTRACT)) {
        modifiers = Abstract +: modifiers
      }
      if (mods.hasFlag(Flag.DEFERRED)) {
        modifiers = Abstract +: modifiers
      }
      if (mods.hasFlag(Flag.SEALED)) {
        modifiers = Sealed +: modifiers
      }
      if (mods.hasFlag(Flag.FINAL)) {
        modifiers = Final +: modifiers
      }
      if (mods.hasFlag(Flag.CASE)) {
        modifiers = Case +: modifiers
      }
      if (mods.hasFlag(Flag.OVERRIDE)) {
        modifiers = Override +: modifiers
      }
      if (mods.hasFlag(Flag.TRAIT)) {
        modifiers = Trait +: modifiers
      }
      if (mods.hasFlag(Flag.MUTABLE)) {
        modifiers = Mutable +: modifiers
      }
      modifiers
    }
  }

  private def isZero(flags: FlagSet): Boolean = {
    flags == 0
  }

  /**
    * Types
    */

  def findType(scope: TScope, typeTree: Tree): TType = {
    def throwTypeNotFound(typeName: String): Nothing = {
      val msg = s"value $typeName not found"
      throw new TypeException(scope.currentFile, typeTree.pos.line, msg)
    }
    typeTree match {
      case s: Select => findClass(scope, s.toString, s.pos.line)
      case rt: RefTree => findClass(scope, rt.name.toString, rt.pos.line)
      case id: ImplDef =>
        val tpeName: String = id.name.toString
        scope.findClass(tpeName).orElse(scope.findObject(tpeName)).getOrElse(throwTypeNotFound(tpeName))
      case l: Literal => findTypeForLiteral(scope, l).getOrElse(throwTypeNotFound(l.value.value.getClass.getName))
      case t: This => scope.findThis()
      case n: New => findType(scope, n.tpt)
      case att: AppliedTypeTree =>
        val appliedTypes = att.args.map(findType(scope, _))
        findType(scope, att.tpt) match {
          case gt: GenericType =>
            if (gt.genericModifiers.size != appliedTypes.size)
              throw new TypeException(scope.currentFile, att.pos.line,
                s"Wrong number of type arguments. Expected ${gt.genericModifiers.size}, but was ${appliedTypes.size}")
            if (gt.genericModifiers == appliedTypes) gt
            else {
              val appliedMap = gt.genericModifiers.zip(appliedTypes).toMap
              gt.applyTypes(appliedMap)
            }
          case _ => throw new TypeException(scope.currentFile, att.pos.line,
            s"Wrong number of type arguments. Expected 0, but was ${appliedTypes.size}")
        }
      case tt: TypeTree => null
      case _@other => throw new RuntimeException(s"Unknown Typetree: ${showRaw(other)}")
    }
  }

  def findClasses(scope: TScope, types: Seq[String]): Seq[TType] = {
    types.map(findClass(scope, _))
  }

  def findClass(scope: TScope, className: String, pos: Int = 0): TType = {
    def throwTypeNotFound(): Nothing = {
      val msg = s"value $className not found"
      throw new TypeException(scope.currentFile, pos, msg)
    }
    scope.findClass(className).getOrElse(throwTypeNotFound())
  }

  private def findTypeForLiteral(scope: TScope, literal: Literal): Option[TType] = {
    val tpeString = literal.value.value match {
      case i: Integer => findTypeForInteger(i)
      case l: java.lang.Long => "scala.Long"
      case s: String => "scala.String"
      case d: java.lang.Double => "scala.Double"
      case b: java.lang.Boolean => "scala.Boolean"
      case c: Character => "scala.Char"
      case u: BoxedUnit => "scala.Unit"
      case null => "scala.Null"
      case _@n => throw new TypeException(scope.currentFile, literal.pos.line, s"literal of type ${n.getClass.getName} not supported")
    }
    if (tpeString != null) scope.findClass(tpeString) else Some(null)
  }

  private def findTypeForInteger(i: Integer): String = {
    //TODO: if the given integer is forced to be a short -> return short, else -> return Int
    if (i >= -128 && i <= 127) {
      "scala.Short" // C++ does not have a byte datatype
    } else if (i >= -32768 && i <= 32767) {
      "scala.Short"
    } else {
      "scala.Int"
    }
  }

  def addClass(scope: TScope, tpe: TType) = {
    scope.addClass(tpe)
  }

  def createAndAddGenericModifiers(scope: TScope, generics: Seq[TypeDef]) = {
    generics.map(createAndAddGenericModifier(scope, _))
  }

  def createAndAddGenericModifier(scope: TScope, generic: TypeDef) = {
    println("generics - generate")
    val genericModifier: GenericModifier = createGenericModifier(scope, generic)
    val millis: Long = System.currentTimeMillis()
    scope.addClass(genericModifier)
    println("Add time: " + (System.currentTimeMillis() - millis))
    genericModifier
  }

  def createGenericModifier(scope: TScope, generic: TypeDef) = {
    val ctx = Context(scope.currentFile, generic.pos.line)
    val (lower, upper) = generic.rhs match {
      case tbt: TypeBoundsTree =>
        val lo = tbt.lo match {
          case EmptyTree => nothingType(scope)
          case _ => TypeUtils.findType(scope, tbt.lo)
        }
        val hi = tbt.hi match {
          case EmptyTree => anyType(scope)
          case _ => TypeUtils.findType(scope, tbt.hi)
        }
        (lo, hi)
    }

    //val upperBound = TypeUtils.findClass(currentScope, )
    val coVariant: Boolean = generic.mods.hasFlag(Flag.COVARIANT)
    val contraVariant: Boolean = generic.mods.hasFlag(Flag.CONTRAVARIANT)
    new GenericModifier(ctx, generic.name.toString, upper, lower, coVariant, contraVariant)
  }

  /**
    * Methods
    */

  def findMethodForDef(scope: TScope, defdef: DefDef, onType: TType = null): Method = {
    val args: List[TType] = defdef.vparamss.head.map {
      case v: ValDef => findType(scope, v.tpt)
    }
    val defName: String = defdef.name.toString
    findMethod(scope, defName, defdef.pos.line, args, onType)
  }

  def findMethodForIdent(scope: TScope, ident: Ident, onType: TType = null): Method = {
    val defName: String = ident.name.toString
    findMethod(scope, defName, ident.pos.line, Vector(), onType)
  }

  def findConstructor(scope: TScope, line: Int, args: Seq[TType], onType: TType = null) = {
    findMethod(scope, ConstructorName, line, args, onType)
  }

  def findMethod(scope: TScope, name: String, line: Int, args: Seq[TType], onType: TType = null,
                 genericModifier: Seq[GenericModifier] = Seq()): Method = {
    val tpe: TType = if (onType == null) scope.findThis() else onType
    tpe.findMethod(scope.findThis(), name, args) getOrElse throwMethodNotFound(scope, name, args, line)
  }

  def applyConstructor(scope: TScope, args: Seq[TType], n: New): TType = {
    val onType: TType = findType(scope, n)
    onType match {
      case at: AppliedGenericType => at
      case gt: GenericType =>
        val method: Method = findConstructor(scope, n.pos.line, args, gt)
        val appliedTypes: Map[GenericModifier, TType] = method.getAppliedTypes(args)
        gt.applyTypes(appliedTypes)
      case _ => onType
    }
  }

  def applyTypesOnType(scope: TScope, onType: TType, appliedTypes: Seq[TType], line: Int): TType = {
    onType match {
      case gt: GenericType =>
        if (gt.genericModifiers.size != appliedTypes.size)
          throw new TypeException(scope.currentFile, line,
            s"Wrong number of type arguments. Expected ${gt.genericModifiers.size}, but was ${appliedTypes.size}")
        else {
          val map = gt.genericModifiers.zip(appliedTypes).map(pair => {
            checkTypeToApply(scope, line, pair._1, pair._2)
            pair
          }).toMap
          gt.applyTypes(map)
        }
      case _ => onType
    }
  }

  def checkTypeToApply(scope: TScope, line: Int, genericModifier: GenericModifier, tpeToApply: TType) = {
    if (!tpeToApply.hasParent(genericModifier.upperBound) || !tpeToApply.isAssignableFrom(genericModifier.lowerBound)) {
      throw new TypeException("", 0, s"Type $tpeToApply is not applicatple for generic modifier $genericModifier")
    }
  }

  private def throwMethodNotFound(scope: TScope, methodName: String, args: Seq[TType], line: Int): Nothing = {
    val argList = TypeUtils.toString(args)
    val msg = s"No method $methodName($argList) found"
    throw new TypeException(scope.currentFile, line, msg)
  }

  def createMethod(scope: TScope, d: DefDef, instanceMethod: Boolean = true): Method = {
    val ctx = Context(scope.currentFile, d.pos.line)
    val methodName: String = d.name.toString
    println("Generics")
    val generics: Seq[GenericModifier] = d.tparams.map(createAndAddGenericModifier(scope, _))
    println("Before Params")
    val params: List[Param] = d.vparamss.head.map {
      case v: ValDef =>
        val tpe: TType = TypeUtils.findType(scope, v.tpt)
        Param(ctx, tpe, v.name.toString, v.rhs != EmptyTree, v.mods.hasFlag(Flag.MUTABLE))
    }
    println("After Params")
    val constructor: Boolean = isConstructor(methodName)
    val retType = if (constructor && !instanceMethod) scope.findThis() else TypeUtils.findType(scope, d.tpt)
    if (retType == null && !constructor) {
      throw new TypeException(ctx.fileName, ctx.line, s"A return type for Method $methodName is required ")
    }
    //TODO check if Method exists in current scope
    Method(ctx, methodName, retType, TypeUtils.getModifiers(d.mods), params, generics, constructor, instanceMethod)
  }

  /**
    * Params
    */

  def addParamsToScope(scope: TScope, params: Seq[Param]) = {
    params.foreach(p => scope.add(Identifier(p.ctx, p.name, p.tpe, p.mutable)))
  }

  def areParamsApplicable(definedParams: Seq[TType], actualParams: Seq[TType]): Boolean = {
    actualParams.zip(definedParams).forall(a => isParamApplicable(a._1, a._2))
  }

  def isParamApplicable(actualParam: TType, definedParam: TType): Boolean = {
    definedParam.isAssignableFrom(actualParam)
  }

  /**
    * Fields
    */

  def findField(scope: TScope, v: ValDef, onType: TType = null): Field = {
    val fieldName: String = v.name.toString
    findField(scope, fieldName, v.pos.line, onType)
  }

  def findField(scope: TScope, fieldName: String, line: Int, onType: TType): Field = {
    def throwFieldNotFound(fieldName: String, tpe: TType): Nothing = {
      val msg = s"value $fieldName is not a member of type ${tpe.name}"
      throw new TypeException(scope.currentFile, line, msg)
    }
    val thisTpe: TType = scope.findThis()
    val tpe: TType = if (onType == null) thisTpe else onType
    tpe.findField(thisTpe, fieldName) getOrElse throwFieldNotFound(fieldName, tpe)
  }

  def isConstructor(methodName: String): Boolean = {
    methodName == "<init>"
  }

  def findMethodOrFieldType(scope: TScope, name: String, line: Int, onType: TType = null) = {
    def throwMethodNotFound(selectName: String): Nothing = {
      val msg = s"No value $selectName found"
      throw new TypeException(scope.currentFile, line, msg)
    }
    val thisTpe: TType = scope.findThis()
    val tpe: TType = if (onType == null) thisTpe else onType
    tpe.findMethod(thisTpe, name, Vector()).map(_.returnType) orElse tpe.findField(thisTpe, name).map(_.tpe) getOrElse throwMethodNotFound(name)
  }

  /**
    * Identifier
    */

  def findIdentifier(scope: TScope, i: Ident): Identifier = {
    def throwIdentifierNotFound(identName: String): Nothing = {
      val msg = s"value $identName not found"
      throw new TypeException(scope.currentFile, i.pos.line, msg)
    }
    val name: String = i.name.toString
    scope.findIdentifier(name) orElse scope.findThis().findField(scope.findThis(), name)
      .map(_.asIdentifier) getOrElse throwIdentifierNotFound(name)
  }

  def createIdentifier(scope: TScope, v: ValDef, givenTpe: TType = null) = {
    val tpe: TType = if (givenTpe != null) givenTpe else TypeUtils.findType(scope, v.tpt)
    val name: String = v.name.toString
    if (scope.findIdentifierInCurrentScope(name).isEmpty) {
      scope.add(Identifier(Context(scope.currentFile, v.pos.line), v.name.toString, tpe, v.mods.hasFlag(Flag.MUTABLE)))
    } else {
      throw new TypeException(scope.currentFile, v.pos.line, s"$name is already defined as value $name")
    }
  }

  /**
    * Member
    */

  def findIdent(scope: TScope, name: String, onType: TType = null, withParams: Seq[TType] = Vector()): (Any) = {
    scope.findMethod(name, withParams) match {
      case Some(m) => m
      case None =>
        scope.findIdentifier(name) match {
          case Some(ident) => ident
          case None =>
            val thisTpe: TType = scope.findThis()
            val tpe = if (onType != null) onType else thisTpe
            tpe.findMethod(thisTpe, name, withParams) match {
              case Some(tMethod) => tMethod
              case None =>
                tpe.findField(thisTpe, name) match {
                  case Some(tField) => tField
                  case None => throw new RuntimeException("Todo")
                }
            }
        }
    }
  }

  def findMember(scope: TScope, name: String, onType: TType = null): Member = {
    val thisTpe: TType = scope.findThis()
    val tpe = if (onType == null) thisTpe else onType
    tpe.findMethod(thisTpe, name, Vector()) match {
      case Some(tMethod) => tMethod
      case None =>
        tpe.findField(thisTpe, name) match {
          case Some(tField) => tField
          case None => throw new RuntimeException("Todo")
        }
    }
  }

  /**
    * Generics
    */

  def getNewTpe(types: Map[GenericModifier, TType], oldType: TType, applyPartly: Boolean = false) = {
    oldType match {
      case g: GenericModifier =>
        types.get(g) match {
          case None => g
          case Some(tpe) => g.applyType(tpe)
        }
      case g: GenericType =>
        val typesToApply = if (applyPartly) findTypesToApply(types, g) else findTypesToApplyPartly(types, g)
        g.applyTypes(typesToApply) // TODO apply partly
      case _ => oldType
    }
  }

  private def findTypesToApply(types: Map[GenericModifier, TType], g: GenericType): Map[GenericModifier, TType] = {
    g.genericModifiers.map(gm => (gm, types.getOrElse(gm, gm.upperBound))).toMap
  }

  private def findTypesToApplyPartly(types: Map[GenericModifier, TType], g: GenericType): Map[GenericModifier, TType] = {
    g.genericModifiers.map(gm => (gm, types.getOrElse(gm, null))).toMap
  }

  def findGenericModifiers(tpe: TType): Seq[GenericModifier] = tpe match {
    case a: AppliedGenericModifier => Vector(a)
    case g: GenericModifier => Vector(g)
    case a: AppliedGenericType => a.appliedTypes.flatMap(findGenericModifiers)
    case g: GenericType => g.genericModifiers
    case c: ConcreteType => Vector()
    case _ => Vector()
  }

  def checkGenericTypes(param: GenericType, arg: GenericType): Boolean = {
    if (param.baseTypeEquals(arg)) {

    }
    if (param.isAssignableFrom(arg)) {
      param match {
        case a: AppliedGenericType => checkGenericTypes2(a, arg)
        case b: GenericType => checkGenericTypes2(b, arg)
      }
    } else {
      false
    }
  }

  def checkGenericTypes2(param: GenericType, arg: GenericType): Boolean = {
    arg match {
      case a: AppliedGenericType => param == a
      case b: GenericType => true
    }
  }

  def checkGenericTypes2(param: AppliedGenericType, arg: GenericType): Boolean = {
    arg match {
      case a: AppliedGenericType => true
      case b: GenericType => true
    }
  }

  /**
   * Utility
   */

  def toString(nameables: Seq[Nameable]) = nameables.map(_.name).mkString(", ")

  def findCommonBaseClass(scope: TScope, tpe1: TType, tpe2: TType): TType = {
    val unit: TType = unitType(scope)
    if (tpe1 == unit || tpe2 == unit) unit
    else {
      var foundType: TType = null
      tpe1.foreachType(t => {
        if (foundType == null && tpe2.hasParent(t)) foundType = t
      })
      foundType
    }
  }

  def findCommonBaseClass(scope: TScope, tpe1: Option[TType], tpe2: Option[TType]): TType = {
    if (tpe1.isEmpty || tpe2.isEmpty) unitType(scope)
    else {
      findCommonBaseClass(scope, tpe1.get, tpe2.get)
    }
  }

  def unitType(scope: TScope) = {
    if (unitTpe == null) {
      unitTpe = scope.findClass("scala.Unit").get
    }
    unitTpe
  }

  def nullType(scope: TScope) = {
    if (nullTpe == null) {
      nullTpe = scope.findClass("scala.Null").get
    }
    nullTpe
  }

  def anyType(scope: TScope) = {
    if (anyTpe == null) {
      anyTpe = scope.findClass("scala.Any").get
    }
    anyTpe
  }

  def nothingType(scope: TScope) = {
    if (nothingTpe == null) {
      nothingTpe = scope.findClass("scala.Nothing").get
    }
    nothingTpe
  }

  def getUsedTypes(baseTypes: BaseTypes, tpe: TType): Set[TType] = {
    val set = new mutable.HashSet[TType]()
    tpe match {
      case agt: AppliedGenericType =>
        agt.appliedTypes.foreach(addTpe(baseTypes, _, set))
      case _ =>
    }
    tpe.fields.foreach(f => addTpe(baseTypes, f.tpe, set))
    tpe.methods.flatMap(_.params.map(_.tpe)).foreach(addTpe(baseTypes, _, set))
    tpe.parentTypes.foreach(addTpe(baseTypes, _, set))
    tpe match {
      case a: AppliedGenericType => set.toSet
      case g: GenericType => set.filter(t => !g.baseTypeEquals(t)).toSet
      case _ => set.toSet
    }
  }

  private def addTpe(baseTypes: BaseTypes, tpe: TType, buffer: mutable.HashSet[TType]): Unit = tpe match {
    case a: AppliedGenericModifier => a.getConcreteType match {
      case g: GenericModifier =>
      case _@t if !baseTypes.isPrimitive(t) => buffer.add(t)
      case _ =>
    }
    case gm: GenericModifier =>
    case at: AppliedGenericType if !baseTypes.isPrimitive(at) =>
      buffer.add(at)
      at.appliedTypes.foreach(addTpe(baseTypes, _, buffer))
    case c: ConcreteType if !baseTypes.isPrimitive(c) => buffer.add(c)
    case _ =>
  }
}
