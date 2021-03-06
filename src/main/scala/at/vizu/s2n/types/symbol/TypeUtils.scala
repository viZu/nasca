package at.vizu.s2n.types.symbol

import at.vizu.s2n.error.{Errors, TypeErrors}
import at.vizu.s2n.exception.TypeException
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 21.10.15.
 */
object TypeUtils extends LazyLogging {

  val RootPackage = "_root_"
  val ScalaPackage = "scala"
  val RootScalaPackage = s"$RootPackage.$ScalaPackage"
  val ConstructorName: String = "<init>"
  val ApplyMethodName: String = "apply"

  private var unitTpe: TType = null
  private var nullTpe: TType = null
  private var anyTpe: TType = null
  private var anyRefTpe: TType = null
  private var anyValTpe: TType = null
  private var nothingTpe: TType = null
  private var anyTpes: Set[TType] = null

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

  def findType(scope: TSymbolTable, typeTree: Tree, searchObject: Boolean = false): TType = {
    def throwTypeNotFound(typeName: String) = {
      val msg = s"value $typeName not found"
      TypeErrors.addError(scope, typeTree.pos.line, msg)
    }
    typeTree match {
      case s: Select => findClass(scope, s.toString, s.pos.line)
      case rt: RefTree => findClass(scope, rt.name.toString, rt.pos.line)
      case id: ImplDef =>
        val tpeName: String = id.name.toString
        if (searchObject) scope.findObject(tpeName).getOrElse(throwTypeNotFound(tpeName))
        else scope.findClass(tpeName).getOrElse(throwTypeNotFound(tpeName))
      case l: Literal => findTypeForLiteral(scope, l).getOrElse(throwTypeNotFound(l.value.value.getClass.getName.replaceAll("java.lang", RootScalaPackage)))
      case t: This => scope.findThis()
      case n: New => findType(scope, n.tpt)
      case att: AppliedTypeTree =>
        findType(scope, att.tpt) match {
          case gt: GenericType =>
            if (gt.genericModifiers.size != att.args.size)
              TypeErrors.addError(scope, att.pos.line,
                s"Wrong number of type arguments. Expected ${gt.genericModifiers.size}, but was ${att.args.size}")
            val appliedTypes = att.args.map(findType(scope, _))
            if (gt.genericModifiers == appliedTypes) gt
            else {
              val appliedMap = gt.genericModifiers.zip(appliedTypes).toMap
              gt.applyTypes(scope, appliedMap)
            }
          case _ => TypeErrors.addError(scope, att.pos.line,
            s"Wrong number of type arguments. Expected 0, but was ${att.args.size}")
        }
      case tt: TypeTree => null
      case _@other => throw new RuntimeException(s"Unknown Typetree: ${showRaw(other)}")
    }
  }

  def findClasses(scope: TSymbolTable, types: Seq[String]): Seq[TType] = {
    types.map(findClass(scope, _))
  }

  def findClass(scope: TSymbolTable, className: String, pos: Int = 0): TType = {
    def throwTypeNotFound() = {
      val msg = s"value $className not found"
      TypeErrors.addError(scope, pos, msg)
    }
    scope.findClass(className).getOrElse(throwTypeNotFound())
  }

  private def findTypeForLiteral(scope: TSymbolTable, literal: Literal): Option[TType] = {
    val tpeString = literal.value.value match {
      case i: Integer => findTypeForInteger(i)
      case l: java.lang.Long => RootScalaPackage + ".Long"
      case s: String => RootScalaPackage + ".String"
      case d: java.lang.Double => RootScalaPackage + ".Double"
      case b: java.lang.Boolean => RootScalaPackage + ".Boolean"
      case c: Character => RootScalaPackage + ".Char"
      case u: BoxedUnit => RootScalaPackage + ".Unit"
      case null => RootScalaPackage + ".Null"
      case _@n =>
        throw new TypeException(scope.currentFile, literal.pos.line, s"literal of type ${n.getClass.getName} not supported")
    }
    if (tpeString != null) scope.findClass(tpeString) else Some(null)
  }

  private def findTypeForInteger(i: Integer): String = {
    //TODO: if the given integer is forced to be a short -> return short, else -> return Int
    if (i >= -128 && i <= 127) {
      RootScalaPackage + ".Short" // C++ does not have a byte datatype
    } else if (i >= -32768 && i <= 32767) {
      RootScalaPackage + ".Short"
    } else {
      RootScalaPackage + ".Int"
    }
  }

  def addClass(scope: TSymbolTable, tpe: TType) = {
    scope.addClass(tpe)
  }

  def createAndAddGenericModifiers(scope: TSymbolTable, generics: Seq[TypeDef]) = {
    generics.map(createAndAddGenericModifier(scope, _))
  }

  def createAndAddGenericModifier(scope: TSymbolTable, generic: TypeDef) = {
    logger.trace("generics - generate")
    val genericModifier: TypeArgument = createGenericModifier(scope, generic)
    val millis: Long = System.currentTimeMillis()
    scope.addTypeArgument(genericModifier)
    logger.trace("Add time: " + (System.currentTimeMillis() - millis))
    genericModifier
  }

  def createGenericModifier(scope: TSymbolTable, generic: TypeDef) = {
    val (lower, upper) = getTypeBounds(scope, generic)
    val coVariant: Boolean = generic.mods.hasFlag(Flag.COVARIANT)
    val contraVariant: Boolean = generic.mods.hasFlag(Flag.CONTRAVARIANT)
    val ctx = Context(scope.currentFile, generic.pos.line)
    new TypeArgument(ctx, generic.name.toString, upper, lower, coVariant, contraVariant)
  }

  def getTypeBounds(scope: TSymbolTable, generic: TypeDef) = {
    generic.rhs match {
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
  }

  /**
    * Methods
    */

  def findMethodForDef(scope: TSymbolTable, defdef: DefDef, onType: TType = null): Method = {
    val args: Seq[TType] = findParamTypes(scope, defdef.vparamss)
    val defName: String = defdef.name.toString
    findMethod(scope, defName, defdef.pos.line, args, onType)
  }

  private def findParamTypes(scope: TSymbolTable, params: Seq[Seq[Tree]]) = {
    if (params.isEmpty) Vector()
    else {
      params.head.map {
        case v: ValDef => findType(scope, v.tpt)
      }
    }
  }

  def findMethodForIdent(scope: TSymbolTable, ident: Ident, onType: TType = null): Method = {
    val defName: String = ident.name.toString
    findMethod(scope, defName, ident.pos.line, Vector(), onType)
  }

  def findConstructor(scope: TSymbolTable, line: Int, args: Seq[TType], onType: TType = null) = {
    findMethod(scope, ConstructorName, line, args, onType)
  }

  def findMethod(scope: TSymbolTable, name: String, line: Int, args: Seq[TType], onType: TType = null,
                 genericModifier: Seq[TypeArgument] = Seq()): Method = {
    val tpe: TType = if (onType == null) scope.findThis() else onType
    tpe.findMethod(scope.findThis(), name, args) getOrElse throwMethodNotFound(scope, name, args, line)
  }

  def applyConstructor(scope: TSymbolTable, args: Seq[TType], n: New): TType = {
    val onType: TType = findType(scope, n)
    onType match {
      case at: AppliedGenericType => at
      case gt: GenericType =>
        val method: Method = findConstructor(scope, n.pos.line, args, gt)
        val appliedTypes: Map[TypeArgument, TType] = method.getAppliedTypes(args)
        gt.applyTypes(scope, appliedTypes)
      case _ => onType
    }
  }

  def applyTypesOnType(scope: TSymbolTable, onType: TType, appliedTypes: Seq[TType], line: Int): TType = {
    onType match {
      case gt: GenericType =>
        if (gt.genericModifiers.size != appliedTypes.size)
          TypeErrors.addError(scope, line,
            s"Wrong number of type arguments. Expected ${gt.genericModifiers.size}, but was ${appliedTypes.size}")
        else {
          val map = gt.genericModifiers.zip(appliedTypes).map(pair => {
            checkTypeToApply(scope, line, pair._1, pair._2)
            pair
          }).toMap
          gt.applyTypes(scope, map)
        }
      case _ => onType
    }
  }

  def checkTypeToApply(scope: TSymbolTable, line: Int, genericModifier: TypeArgument, tpeToApply: TType) = {
    if (!tpeToApply.hasParent(genericModifier.upperBound) || !tpeToApply.isAssignableFrom(genericModifier.lowerBound)) {
      TypeErrors.addError(scope, line, s"Type $tpeToApply is not applicatple for generic modifier $genericModifier")
    }
  }

  private def throwMethodNotFound(scope: TSymbolTable, methodName: String, args: Seq[TType], line: Int) = {
    Errors.validate(s => "")
    val argList = TypeUtils.toString(args)
    val msg = s"No method $methodName($argList) found"
    throw new TypeException(scope.currentFile, line, msg)
  }

  def createMethod(scope: TSymbolTable, d: DefDef, instanceMethod: Boolean = true, primaryConstructor: Boolean = false): Method = {
    val ctx = Context(scope.currentFile, d.pos.line)
    val methodName: String = d.name.toString
    val typeArguments: Seq[TypeArgument] = d.tparams.map(createAndAddGenericModifier(scope, _))
    val params: Seq[Param] = createParamsForMethod(scope, d.vparamss)
    val constructor: Boolean = isConstructor(methodName)
    val retType = if (constructor && !instanceMethod) scope.findThis() else TypeUtils.findType(scope, d.tpt)
    if (retType == null && !constructor) {
      TypeErrors.addError(ctx, s"A return type for Method $methodName is required ")
    }
    //TODO check if Method exists in current scope
    if (constructor) Constructor(ctx, retType, TypeUtils.getModifiers(d.mods), params, primaryConstructor)
    else Method(ctx, methodName, retType, TypeUtils.getModifiers(d.mods), params, typeArguments, instanceMethod)
  }

  def createParamsForMethod(scope: TSymbolTable, params: Seq[Seq[Tree]]): Seq[Param] = {
    if (params.isEmpty) Vector()
    else createParams(scope, params.head)
  }

  /**
    * Params
    */

  def createParams(scope: TSymbolTable, params: Seq[Tree]) = {
    params.map(createParam(scope, _))
  }

  def createParam(scope: TSymbolTable, param: Tree) = param match {
    case v: ValDef =>
      val ctx = Context(scope.currentFile, v.pos.line)
      val tpe: TType = TypeUtils.findType(scope, v.tpt)
      Param(ctx, tpe, v.name.toString, v.rhs != EmptyTree, v.mods.hasFlag(Flag.MUTABLE))
  }

  def addReflectParamsToScope(scope: TSymbolTable, params: Seq[ValDef]) = {
    params.foreach(createIdentifier(scope, _))
  }

  def addParamsToScope(scope: TSymbolTable, params: Seq[Param]) = {
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

  def findField(scope: TSymbolTable, v: ValDef, onType: TType = null): Field = {
    val fieldName: String = v.name.toString
    findField(scope, fieldName, v.pos.line, onType)
  }

  def findField(scope: TSymbolTable, fieldName: String, line: Int, onType: TType): Field = {
    def throwFieldNotFound(fieldName: String, tpe: TType) = {
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

  def findMethodOrFieldType(scope: TSymbolTable, name: String, line: Int, onType: TType = null) = {
    def throwMethodNotFound(selectName: String) = {
      val msg = s"No value $selectName found"
      TypeErrors.addError(scope, line, msg)
    }
    val thisTpe: TType = scope.findThis()
    val tpe: TType = if (onType == null) thisTpe else onType
    tpe.findMethod(thisTpe, name, Vector()).map(_.returnType) orElse tpe.findField(thisTpe, name).map(_.tpe) getOrElse throwMethodNotFound(name)
  }

  /**
    * Identifier
    */

  def findIdentifier(scope: TSymbolTable, i: Ident): Identifier = {
    def throwIdentifierNotFound(identName: String) = {
      val msg = s"value $identName not found"
      throw new TypeException(scope.currentFile, i.pos.line, msg)
    }
    val name: String = i.name.toString
    val thisTpe: TType = scope.findThis()
    scope.findIdentifier(name) orElse thisTpe.findField(thisTpe, name)
      .map(_.asIdentifier) getOrElse throwIdentifierNotFound(name)
  }

  def createIdentifier(scope: TSymbolTable, v: ValDef, givenTpe: TType = null) = {
    val tpe: TType = if (givenTpe != null) givenTpe else TypeUtils.findType(scope, v.tpt)
    val name: String = v.name.toString
    if (scope.findIdentifierInCurrentScope(name).isEmpty) {
      scope.add(Identifier(Context(scope.currentFile, v.pos.line), v.name.toString, tpe, v.mods.hasFlag(Flag.MUTABLE)))
    } else {
      TypeErrors.addError(scope.currentFile, v.pos.line, s"$name is already defined as value $name")
    }
  }

  /**
    * Member
    */

  def findIdent(scope: TSymbolTable, name: String, onType: TType = null, withParams: Seq[TType] = Vector()): Identifiable = {
    scope.findIdentifier(name) match {
      case Some(ident) if onType == null => ident
      case _ =>
        scope.findMethod(name, withParams) match {
          case Some(m) if onType == null => m
          case _ =>
            val thisTpe: TType = scope.findThis()
            val tpe = if (onType != null) onType else thisTpe
            tpe.findMethod(thisTpe, name, withParams) match {
              case Some(tMethod) => tMethod
              case None =>
                tpe.findField(thisTpe, name) match {
                  case Some(tField) => tField
                  case None =>
                    scope.findObject(name) match {
                      case Some(obj) => obj
                      case None => TypeErrors.addError(scope, 0, s"value $name not found")
                    }
                }
            }
        }
    }
  }

  def findMember(scope: TSymbolTable, name: String, onType: TType = null): Member = {
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

  def getNewTpe(scope: TSymbolTable, types: Map[TypeArgument, TType], oldType: TType, applyPartly: Boolean = false) = {
    oldType match {
      case g: TypeArgument =>
        types.get(g) match {
          case None => g
          case Some(tpe) => g.applyType(tpe)
        }
      case g: GenericType =>
        val typesToApply = if (applyPartly) findTypesToApply(types, g) else findTypesToApplyPartly(types, g)
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

  def findGenericModifiers(tpe: TType): Seq[TypeArgument] = tpe match {
    case a: AppliedTypeArgument => Vector(a)
    case g: TypeArgument => Vector(g)
    case a: AppliedGenericType => Vector() //a.appliedTypes.flatMap(findGenericModifiers)
    case g: GenericType => Vector() //g.genericModifiers
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

  def addTypeArgumentsToScope(scope: TSymbolTable, generics: Seq[TypeDef]): Unit = {
    generics.map(createGenericModifier(scope, _)).foreach(gm => {
      scope.addTypeArgument(gm)
    })
  }

  def addGenericModifiersToScope(scope: TSymbolTable, t: TType): Unit = {
    t match {
      case a: AppliedGenericType =>
      case g: GenericType => g.genericModifiers.foreach(scope.addTypeArgument)
      case _ =>
    }
  }

  /**
    * Functions
    */

  def methodAsFunctionType(scope: TSymbolTable, method: Method, line: Int) = {
    createFunctionTypeFromParams(scope, method.params, method.tpe, line)
  }

  def createFunctionTypeFromParams(scope: TSymbolTable, params: Seq[Param], retType: TType, line: Int) = {
    if (retType == null) createFunctionType(scope, params.map(_.tpe), unitType(scope), line)
    else createFunctionType(scope, params.map(_.tpe), retType, line)
  }

  def createFunctionType(scope: TSymbolTable, paramTypes: Seq[TType], retType: TType, line: Int) = {
    val funcName = "Function" + paramTypes.size
    val funcType = findClass(scope, funcName, line).asInstanceOf[GenericType]
    val typeMap = funcType.genericModifiers.zip(paramTypes :+ retType).toMap
    funcType.applyTypes(scope, typeMap)
  }

  def getFunctionReturnType(tpe: TType) = tpe match {
    case a: AppliedGenericType if isFunctionType(tpe) => a.appliedTypes.last
    case g: GenericType if isFunctionType(tpe) => g.genericModifiers.last
  }

  def isFunctionType(tpe: TType) = {
    tpe.fullClassName.startsWith(TypeUtils.RootScalaPackage + ".Function")
  }

  /**
   * Utility
   */

  def toString(nameables: Seq[Nameable]) = nameables.map(_.name).mkString(", ")

  def findCommonBaseClass(scope: TSymbolTable, tpes: Seq[TType]): TType = {
    def findCommonBaseClassAcc(tpe: TType, nextTypes: Seq[TType]): TType = {
      nextTypes match {
        case Seq() => tpe
        case head +: tail => findCommonBaseClassAcc(findCommonBaseClass(scope, tpe, head), tail)
      }
    }
    tpes match {
      case Seq() => nothingType(scope)
      case head +: tail => findCommonBaseClassAcc(head, tail)
    }
  }

  def findCommonBaseClass(scope: TSymbolTable, tpe1: TType, tpe2: TType): TType = {
    def handleGenericModifiers(found1: TType, found2: TType): TType = {
      def getConcreteType(tpe: TType) = {
        tpe match {
          case a: AppliedTypeArgument => a.getConcreteType
          case _ => tpe
        }
      }
      def findGenericModifiers(gen1: GenericType, gen2: GenericType) = {
        def getAppliedTypes(tpe: GenericType) = tpe match {
          case a: AppliedGenericType => a.appliedTypes.map(getConcreteType(_))
          case g: GenericType => g.genericModifiers
        }
        getAppliedTypes(gen1).zip(getAppliedTypes(gen2)).map(pair => findCommonBaseClass(scope, pair._1, pair._2))
      }
      val found1Concrete = getConcreteType(found1)
      val found2Concrete = getConcreteType(found2)
      found1Concrete match {
        case g: GenericType =>
          val genericModifiers: Seq[TType] = findGenericModifiers(g, found2Concrete.asInstanceOf[GenericType])
          g.genericType.applyTypeSeq(scope, genericModifiers)
        case _ => throw new RuntimeException()
      }
    }
    val unit: TType = unitType(scope)
    val nullT = nullType(scope)
    if (tpe1 == unit || tpe2 == unit) unit
    else {
      val foundType: TType = findSimpleCommonBaseClass(tpe1, tpe2)
      if (isAnyType(scope, foundType)) {
        var types: (TType, TType) = null
        tpe1.foreachType(t1 => {
          tpe2.foreachType(t2 => {
            if (types == null && t1.baseTypeEquals(t2)) types = (t1, t2)
          })
        })
        handleGenericModifiers(types._1, types._2)
      } else foundType
    }
  }

  private def findSimpleCommonBaseClass(tpe1: TType, tpe2: TType): TType = {
    if (tpe1 == nullTpe) tpe2
    else if (tpe2 == nullTpe) tpe1
    else {
      var foundType: TType = null
      tpe1.foreachType(t => {
        if (foundType == null && tpe2.hasParent(t)) foundType = t
      })
      foundType
    }
  }

  def findCommonBaseClass(scope: TSymbolTable, tpe1: Option[TType], tpe2: Option[TType]): TType = {
    if (tpe1.isEmpty || tpe2.isEmpty) unitType(scope)
    else {
      findCommonBaseClass(scope, tpe1.get, tpe2.get)
    }
  }

  def unitType(scope: TSymbolTable) = {
    if (unitTpe == null) {
      unitTpe = scope.findClass(RootScalaPackage + ".Unit").get
    }
    unitTpe
  }

  def nullType(scope: TSymbolTable) = {
    if (nullTpe == null) {
      nullTpe = scope.findClass(RootScalaPackage + ".Null").get
    }
    nullTpe
  }

  def anyType(scope: TSymbolTable) = {
    if (anyTpe == null) {
      anyTpe = scope.findClass(RootScalaPackage + ".Any").get
    }
    anyTpe
  }

  def anyRefType(scope: TSymbolTable) = {
    if (anyRefTpe == null) {
      anyRefTpe = scope.findClass(RootScalaPackage + ".AnyRef").get
    }
    anyRefTpe
  }

  def anyValType(scope: TSymbolTable) = {
    if (anyValTpe == null) {
      anyValTpe = scope.findClass(RootScalaPackage + ".AnyVal").get
    }
    anyValTpe
  }

  def nothingType(scope: TSymbolTable) = {
    if (nothingTpe == null) {
      nothingTpe = scope.findClass(RootScalaPackage + ".Nothing").get
    }
    nothingTpe
  }

  def isAnyType(scope: TSymbolTable, tpe: TType) = {
    if (anyTpes == null) {
      anyTpes = Set(anyType(scope), anyRefType(scope), anyValType(scope))
    }
    anyTpes.contains(tpe)
  }

  def getUsedTypes(baseTypes: BaseTypes, tpe: TType): Set[TType] = {
    val set = new mutable.HashSet[TType]()
    tpe match {
      case agt: AppliedGenericType =>
        agt.appliedTypes.foreach(addTpe(baseTypes, _, set))
      case _ =>
    }
    tpe.fields.foreach(f => addTpe(baseTypes, f.tpe, set))
    tpe.methods.flatMap(m => m.params.map(_.tpe) :+ m.tpe).foreach(addTpe(baseTypes, _, set))
    tpe.parentTypes.foreach(addTpe(baseTypes, _, set))
    tpe match {
      case a: AppliedGenericType => set.toSet
      case g: GenericType => set.filter(t => !g.baseTypeEquals(t)).toSet
      case _ => set.toSet
    }
  }

  private def addTpe(baseTypes: BaseTypes, tpe: TType, buffer: mutable.HashSet[TType]): Unit = tpe match {
    case a: AppliedTypeArgument => a.getConcreteType match {
      case g: TypeArgument =>
      case _@t if !baseTypes.isPrimitive(t) => buffer.add(t)
      case _ =>
    }
    case gm: TypeArgument =>
    case at: AppliedGenericType if !baseTypes.isPrimitive(at) =>
      buffer.add(at)
      at.appliedTypes.foreach(addTpe(baseTypes, _, buffer))
    case c: ConcreteType if !baseTypes.isPrimitive(c) => buffer.add(c)
    case _ =>
  }
}
