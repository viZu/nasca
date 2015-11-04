package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException

import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit

/**
 * Phil on 21.10.15.
 */
object TypeUtils {

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
      if (mods.hasFlag(Flag.PRIVATE)) {
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
      case id: ImplDef => scope.findClass(id.name.toString).getOrElse(throwTypeNotFound(id.name.toString))
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
      case i: Integer => "scala.Int"
      case s: String => throw new TypeException(scope.currentFile, literal.pos.line, s"literal of type String not supported")
      case d: java.lang.Double => "scala.Double"
      case b: java.lang.Boolean => "scala.Boolean"
      case c: Character => "scala.Char"
      case u: BoxedUnit => "scala.Unit"
      case _@n => throw new TypeException(scope.currentFile, literal.pos.line, s"literal of type ${n.getClass.getName} not supported")
    }
    scope.findClass(tpeString)
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
      val msg = s"No method $methodName with arguments $argList found"
      throw new TypeException(scope.currentFile, line, msg)
    }
    val tpe: TType = if (onType == null) scope.findThis() else onType
    tpe.findMethod(name, args) getOrElse throwMethodNotFound(name)
  }

  def createMethod(scope: TScope, d: DefDef): Method = {
    val ctx = Context(scope.currentFile, d.pos.line)
    val params: List[Param] = d.vparamss.head.map {
      case v: ValDef =>
        val tpe: TType = TypeUtils.findType(scope, v.tpt)
        Param(ctx, tpe, v.name.toString, v.rhs != EmptyTree, v.mods.hasFlag(Flag.MUTABLE))
    }
    val retType = TypeUtils.findType(scope, d.tpt)
    //TODO check if Method exists in current scope
    Method(ctx, d.name.toString, retType, TypeUtils.getModifiers(d.mods), params, TypeUtils.isConstructor(d.name.toString))
  }

  /**
   * Fields
   */

  def findField(scope: TScope, v: ValDef, onType: TType = null) = {
    def throwFieldNotFound(fieldName: String): Nothing = {
      val msg = s"field $fieldName not found"
      throw new TypeException(scope.currentFile, v.pos.line, msg)
    }
    val tpe: TType = if (onType == null) scope.findThis() else onType
    val fieldName: String = v.name.toString
    tpe.findField(fieldName) getOrElse throwFieldNotFound(fieldName)
  }

  def isConstructor(methodName: String): Boolean = {
    methodName == "<init>"
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

  def createIdentifier(scope: TScope, v: ValDef) = {
    val tpe: TType = TypeUtils.findType(scope, v.tpt)
    val name: String = v.name.toString
    if (scope.findIdentifierInCurrentScope(name).isEmpty) {
      scope.add(Identifier(Context(scope.currentFile, v.pos.line), v.name.toString, tpe, v.mods.hasFlag(Flag.MUTABLE)))
    } else {
      throw new TypeException(scope.currentFile, v.pos.line, s"$name is already defined as value $name")
    }
  }

  /**
   * Utility
   */

  def toString(nameables: Seq[Nameable]) = nameables.map(_.name).mkString(", ")

  def findCommonBaseClass(scope: TScope, tpe1: TType, tpe2: TType): TType = {
    val unitTpe: TType = scope.findClass("scala.Unit").get
    if (tpe1 == unitTpe || tpe2 == unitTpe) unitTpe
    else {
      var foundType: TType = null
      tpe1.forEachType(t => {
        if (foundType == null && tpe2.hasParent(t)) foundType = t
      })
      foundType
    }
  }

  def findCommonBaseClass(scope: TScope, tpe1: Option[TType], tpe2: Option[TType]): TType = {
    val unitTpe: TType = scope.findClass("scala.Unit").get
    if (tpe1.isEmpty || tpe2.isEmpty) unitTpe
    else {
      findCommonBaseClass(scope, tpe1.get, tpe2.get)
    }
  }
}
