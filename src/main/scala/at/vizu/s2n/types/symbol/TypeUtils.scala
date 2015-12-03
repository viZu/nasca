package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException

import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 21.10.15.
 */
object TypeUtils {

  private var unitTpe: TType = null
  private var nullTpe: TType = null

  /**
    * Modifiers
    */

  def getModifiers(mods: Modifiers): Seq[Modifier] = {
    var modifiers: List[Modifier] = List[Modifier]()

    if (isZero(mods.flags)) {
      modifiers
    } else {
      if (mods.privateWithin.toString != "") {
        //TODO which package?
        modifiers = PackagePrivate :: modifiers
      }
      if (mods.hasFlag(Flag.PRIVATE) && !mods.hasFlag(Flag.PARAMACCESSOR)) {
        // TODO PARAMACCESSOR -> generate Getter/Setter?
        modifiers = Private :: modifiers
      }
      if (mods.hasFlag(Flag.PROTECTED)) {
        modifiers = Protected :: modifiers
      }
      if (mods.hasFlag(Flag.ABSTRACT)) {
        modifiers = Abstract :: modifiers
      }
      if (mods.hasFlag(Flag.DEFERRED)) {
        modifiers = Abstract :: modifiers
      }
      if (mods.hasFlag(Flag.SEALED)) {
        modifiers = Sealed :: modifiers
      }
      if (mods.hasFlag(Flag.FINAL)) {
        modifiers = Final :: modifiers
      }
      if (mods.hasFlag(Flag.CASE)) {
        modifiers = Case :: modifiers
      }
      if (mods.hasFlag(Flag.OVERRIDE)) {
        modifiers = Override :: modifiers
      }
      if (mods.hasFlag(Flag.TRAIT)) {
        modifiers = Trait :: modifiers
      }
      if (mods.hasFlag(Flag.MUTABLE)) {
        modifiers = Mutable :: modifiers
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
      case rt: RefTree => scope.findClass(rt.name.toString).getOrElse(throwTypeNotFound(rt.name.toString))
      case id: ImplDef =>
        val tpeName: String = id.name.toString
        scope.findClass(tpeName).orElse(scope.findObject(tpeName)).getOrElse(throwTypeNotFound(tpeName))
      case l: Literal => findTypeForLiteral(scope, l).getOrElse(throwTypeNotFound(l.value.value.getClass.getName))
      case t: This => scope.findThis()
      case n: New => findType(scope, n.tpt)
      case att: AppliedTypeTree => throw new RuntimeException(s"Generic Types not supported yet: $att")
      case tt: TypeTree => null
      case _@other => throw new RuntimeException(s"Unknown Typetree: ${showRaw(other)}")
    }
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
    nullType(scope).addParent(tpe)
    scope.addClass(tpe)
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
    findMethod(scope, defName, ident.pos.line, Seq(), onType)
  }

  def findMethod(scope: TScope, name: String, line: Int, args: Seq[TType], onType: TType = null): Method = {
    def throwMethodNotFound(methodName: String): Nothing = {
      val argList = TypeUtils.toString(args)
      val msg = s"No method $methodName($argList) found"
      throw new TypeException(scope.currentFile, line, msg)
    }
    val tpe: TType = if (onType == null) scope.findThis() else onType
    tpe.findMethod(name, args) getOrElse throwMethodNotFound(name)
  }

  def createMethod(scope: TScope, d: DefDef, instanceMethod: Boolean = true): Method = {
    val ctx = Context(scope.currentFile, d.pos.line)
    val params: List[Param] = d.vparamss.head.map {
      case v: ValDef =>
        val tpe: TType = TypeUtils.findType(scope, v.tpt)
        Param(ctx, tpe, v.name.toString, v.rhs != EmptyTree, v.mods.hasFlag(Flag.MUTABLE))
    }

    val methodName: String = d.name.toString
    val retType = TypeUtils.findType(scope, d.tpt)
    if (retType == null && !isConstructor(methodName)) {
      throw new TypeException(ctx.fileName, ctx.line, s"A return type for Method $methodName is required ")
    }
    //TODO check if Method exists in current scope
    Method(ctx, methodName, retType, TypeUtils.getModifiers(d.mods), params, isConstructor(methodName), instanceMethod)
  }

  def addParamsToScope(scope: TScope, params: Seq[Param]) = {
    params.foreach(p => scope.add(Identifier(p.ctx, p.name, p.tpe, p.mutable)))
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
    val tpe: TType = if (onType == null) scope.findThis() else onType
    tpe.findField(fieldName) getOrElse throwFieldNotFound(fieldName, tpe)
  }

  def isConstructor(methodName: String): Boolean = {
    methodName == "<init>"
  }

  def findMethodOrFieldType(scope: TScope, name: String, line: Int, onType: TType = null) = {
    def throwMethodNotFound(selectName: String): Nothing = {
      val msg = s"No value $selectName found"
      throw new TypeException(scope.currentFile, line, msg)
    }
    val tpe: TType = if (onType == null) scope.findThis() else onType
    tpe.findMethod(name, Seq()).map(_.returnType) orElse tpe.findField(name).map(_.tpe) getOrElse throwMethodNotFound(name)
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
    scope.findIdentifier(name) orElse scope.findThis().findField(name)
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

  def findIdent(scope: TScope, name: String, onType: TType = null): (Any) = {
    scope.findMethod(name, Seq()) match {
      case Some(m) => m
      case None =>
        scope.findIdentifier(name) match {
          case Some(ident) => ident
          case None =>
            val tpe = if (onType != null) onType else scope.findThis()
            tpe.findMethod(name, Seq()) match {
              case Some(tMethod) => tMethod
              case None =>
                tpe.findField(name) match {
                  case Some(tField) => tField
                  case None => throw new RuntimeException("Todo")
                }
            }
        }
    }
  }

  def findMember(scope: TScope, name: String, onType: TType = null): Member = {
    val tpe = if (onType == null) scope.findThis() else onType
    tpe.findMethod(name, Seq()) match {
      case Some(tMethod) => tMethod
      case None =>
        tpe.findField(name) match {
          case Some(tField) => tField
          case None => throw new RuntimeException("Todo")
        }
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
      tpe1.forEachType(t => {
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
}
