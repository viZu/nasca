package at.vizu.s2n.types.symbol

import at.vizu.s2n.types.symbol.TypeUtils._

/**
 * Phil on 16.10.15.
 */
class ScalaScopeInitializer extends ScopeInitializer with BaseTypes {

  lazy val ctx: Context = Context("", 0)
  lazy val any = new ConcreteType(_simpleName = "Any", _pkg = RootScalaPackage)
  lazy val anyVal = new ConcreteType(_simpleName = "AnyVal", _pkg = RootScalaPackage)
  lazy val anyRef = new ConcreteType(_simpleName = "AnyRef", _pkg = RootScalaPackage)
  lazy val string = new ConcreteType(_simpleName = "String", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val numeric = new ConcreteType(_simpleName = "NumericPrimitive", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val unit = new ConcreteType(_simpleName = "Unit", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val boolean = new ConcreteType(_simpleName = "Boolean", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val byte = new ConcreteType(_simpleName = "Byte", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val short = new ConcreteType(_simpleName = "Short", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val char = new ConcreteType(_simpleName = "Char", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val int = new ConcreteType(_simpleName = "Int", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val long = new ConcreteType(_simpleName = "Long", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val float = new ConcreteType(_simpleName = "Float", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val double = new ConcreteType(_simpleName = "Double", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val nullTpe = new ConcreteType(_simpleName = "Null", _pkg = RootScalaPackage, _mods = Vector(Trait))
  lazy val nothing = new ConcreteType(_simpleName = "Nothing", _pkg = RootScalaPackage, _mods = Vector(Trait))

  lazy val primitives = Set[TType](boolean, byte, short, char, int, long, float, double, unit, string) // String is primitive

  override def initScope: TScope = {
    val scope: TScope = TScope(this)
    val a: TType = initAny()
    val ar: TType = initAnyRef()
    val av: TType = initAnyVal()

    val anys = Vector(a, ar, av)
    val str = initString()
    val numericPrimitive = initNumericPrimitive()
    val pris = initPrimitives()
    val nullT = initNull()
    val nothing = initNothing((pris :+ nullT).map(Parent(_)))
    val allTypes = anys ++ pris :+ numericPrimitive :+ str :+ nullT :+ nothing

    scope.addAllClasses(allTypes)

    allTypes foreach (t => {
      scope.addTypeAlias(t.simpleName, t.name)
    })

    initRootMethods.foreach(scope.addMethod)
    addFunctionTypes(scope, 6)

    scope
  }

  private def initRootMethods = {
    val println1 = Method(ctx, "println", unit, Vector(Final), Vector(), instanceMethod = false)
    val println2 = Method(ctx, "println", unit, Vector(Final), Vector(any), instanceMethod = false)
    val print = Method(ctx, "print", unit, Vector(Final), Vector(any), instanceMethod = false)
    Vector(println1, println2, print)
  }

  private def initAny() = {
    //TODO Context
    any.addMethod(Method(ctx, "$bang$eq", boolean, Vector(Final), Vector(Param(ctx, any, "arg0")), operator = true))
    any.addMethod(Method(ctx, "$eq$eq", boolean, Vector(Final), Vector(Param(ctx, any, "arg0")), operator = true))
    any.addMethod(Method(ctx, "equals", boolean, Vector(), Vector(Param(ctx, any, "arg0"))))
    any.addMethod(Method(ctx, "hashCode", int, Vector(), Vector()))
    any.addMethod(Method(ctx, "toString", string, Vector(), Vector()))
    any
  }

  private def initAnyVal() = {
    //TODO Context
    anyVal._parents = Vector(Parent(any))
    anyVal
  }

  private def initAnyRef() = {
    //TODO Context
    anyRef._parents = Vector(Parent(any))
    anyRef.addMethod(Method(ctx, "eq", boolean, Vector(Final), Vector(Param(ctx, anyRef, "arg0"))))
    anyRef.addMethod(Method(ctx, "ne", boolean, Vector(Final), Vector(Param(ctx, anyRef, "arg0"))))
    anyRef
  }

  private def initString() = {
    string._parents = Vector(Parent(anyRef))
    string.addMethod(Method(ctx, "$times", string, Vector(Abstract), Vector(Param(ctx, string, "x")), operator = true))
    string.addMethod(Method(ctx, "$plus", string, Vector(Abstract), Vector(Param(ctx, string, "x")), operator = true))
    string.addMethod(Method(ctx, "length", int, Vector(Abstract), nonPointer = true))
    string
  }

  private def initNumericPrimitive() = {
    numeric._parents = Vector()
    numeric.addMethod(Method(ctx, "$less", boolean, Vector(Abstract), Vector(Param(ctx, numeric, "x")), operator = true))
    numeric.addMethod(Method(ctx, "$less$eq", boolean, Vector(Abstract), Vector(Param(ctx, numeric, "x")), operator = true))
    numeric.addMethod(Method(ctx, "$greater", boolean, Vector(Abstract), Vector(Param(ctx, numeric, "x")), operator = true))
    numeric.addMethod(Method(ctx, "$greater$eq", boolean, Vector(Abstract), Vector(Param(ctx, numeric, "x")), operator = true))
    numeric.addMethod(Method(ctx, "toByte", byte, Vector(Abstract), Vector(), operator = true))
    numeric.addMethod(Method(ctx, "toShort", short, Vector(Abstract), Vector(), operator = true))
    numeric.addMethod(Method(ctx, "toChar", char, Vector(Abstract), Vector(), operator = true))
    numeric.addMethod(Method(ctx, "toInt", int, Vector(Abstract), Vector(), operator = true))
    numeric.addMethod(Method(ctx, "toLong", long, Vector(Abstract), Vector(), operator = true))
    numeric.addMethod(Method(ctx, "toFloat", float, Vector(Abstract), Vector(), operator = true))
    numeric.addMethod(Method(ctx, "toDouble", double, Vector(Abstract), Vector(), operator = true))
    numeric
  }

  private def initPrimitives(): Seq[TType] = {
    val d = initDouble()
    val f = initFloat()
    val l = initLong()
    val i = initInt()
    val c = initChar()
    val s = initShort()
    val by = initByte()
    val b = initBoolean()
    val u = initUnit()
    Vector(d, f, l, i, c, s, by, b, u)
  }

  private def initUnit() = {
    //TODO Context
    unit._parents = Vector(Parent(anyVal))
    unit
  }

  private def initBoolean() = {
    //TODO Context
    boolean._parents = Vector(Parent(anyVal))
    boolean.addMethod(Method(ctx, "$less", boolean, Vector(Abstract), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$less$eq", boolean, Vector(Abstract), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$greater", boolean, Vector(Abstract), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$greater$eq", boolean, Vector(Abstract), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$bang$eq", boolean, Vector(Abstract, Override), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$eq$eq", boolean, Vector(Abstract, Override), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$amp$amp", boolean, Vector(Abstract), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "$bar$bar", boolean, Vector(Abstract), Vector(Param(ctx, boolean, "x")), operator = true))
    boolean.addMethod(Method(ctx, "unary_$bang", boolean, Vector(Abstract), Vector(), operator = true))
    boolean
  }

  private def initByte() = {
    //TODO Context
    byte._parents = Vector(Parent(short))
    byte.addMethod(Method(ctx, "unary_$minus", int, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(byte, double, double)
    calcMethods(byte, float, float)
    calcMethods(byte, long, long)
    calcMethods(byte, int, int)
    calcMethods(byte, char, int)
    calcMethods(byte, short, int)
    calcMethods(byte, byte, int)
    byte
  }

  private def initShort() = {
    //TODO Context
    short._parents = Vector(Parent(int))
    short.addMethod(Method(ctx, "unary_$minus", int, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(short, double, double)
    calcMethods(short, float, float)
    calcMethods(short, long, long)
    calcMethods(short, int, int)
    calcMethods(short, char, int)
    calcMethods(short, short, int)
    calcMethods(short, byte, int)
    short
  }

  private def initChar() = {
    //TODO Context
    char._parents = Vector(Parent(int))
    char.addMethod(Method(ctx, "unary_$minus", int, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(char, double, double)
    calcMethods(char, float, float)
    calcMethods(char, long, long)
    calcMethods(char, int, int)
    calcMethods(char, char, int)
    calcMethods(char, short, int)
    calcMethods(char, byte, int)
    char
  }

  private def initInt() = {
    //TODO Context
    int._parents = Vector(Parent(long))
    int.addMethod(Method(ctx, "unary_$minus", int, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(int, double, double)
    calcMethods(int, float, float)
    calcMethods(int, long, long)
    calcMethods(int, int, int)
    calcMethods(int, char, int)
    calcMethods(int, short, int)
    calcMethods(int, byte, int)
    int
  }

  private def initLong() = {
    //TODO Context
    long._parents = Vector(Parent(float))
    long.addMethod(Method(ctx, "unary_$minus", long, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(long, double, double)
    calcMethods(long, float, float)
    calcMethods(long, long, long)
    calcMethods(long, int, long)
    calcMethods(long, char, long)
    calcMethods(long, short, long)
    calcMethods(long, byte, long)
    long
  }

  private def initFloat() = {
    float._parents = Vector(Parent(double))
    float.addMethod(Method(ctx, "unary_$minus", float, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(float, double, double)
    calcMethods(float, float, float)
    calcMethods(float, long, float)
    calcMethods(float, int, float)
    calcMethods(float, char, float)
    calcMethods(float, short, float)
    calcMethods(float, byte, float)
    float
  }

  private def initDouble() = {
    //TODO Context
    double._parents = Vector(anyVal, numeric).map(Parent(_))
    double.addMethod(Method(ctx, "unary_$minus", double, Vector(Abstract, Override), Vector(), operator = true))
    calcMethods(double, double, double)
    calcMethods(double, float, double)
    calcMethods(double, long, double)
    calcMethods(double, int, double)
    calcMethods(double, char, double)
    calcMethods(double, short, double)
    calcMethods(double, byte, double)
    double
  }

  private def calcMethods(appendTo: ConcreteType, tpe: TType, retTpe: TType) = {
    appendTo.addMethod(Method(ctx, "$times", retTpe, Vector(Abstract, Override), Vector(Param(ctx, tpe, "x")), operator = true))
    appendTo.addMethod(Method(ctx, "$plus", retTpe, Vector(Abstract, Override), Vector(Param(ctx, tpe, "x")), operator = true))
    appendTo.addMethod(Method(ctx, "$div", retTpe, Vector(Abstract, Override), Vector(Param(ctx, tpe, "x")), operator = true))
    appendTo.addMethod(Method(ctx, "$minus", retTpe, Vector(Abstract, Override), Vector(Param(ctx, tpe, "x")), operator = true))
    appendTo.addMethod(Method(ctx, "$percent", retTpe, Vector(Abstract, Override), Vector(Param(ctx, tpe, "x")), operator = true))
  }

  private def initNull() = {
    nullTpe._parents = Vector(Parent(string))
    nullTpe
  }

  private def initNothing(parents: Seq[Parent]) = {
    nothing._parents = parents
    nothing
  }

  private def createGenericModifier(name: String, covariant: Boolean = false, contravariant: Boolean = false) = {
    new GenericModifier(ctx, name, any, nothing, covariant, contravariant)
  }

  private def addFunctionTypes(scope: TScope, num: Int) = {
    val types = (0 until num).map(i => createFunctionType(i))
    types.foreach(c => {
      scope.addClass(c)
      scope.addTypeAlias(c.simpleName, c.fullClassName)
    })
  }

  private def createFunctionType(paramSize: Int) = {
    val paramTypes = (1 to paramSize).map(i => createGenericModifier("T" + i, contravariant = true))
    val returnType = createGenericModifier("R", covariant = true)
    val fType = new GenericType(_simpleName = "Function" + paramSize, _pkg = RootScalaPackage, _mods = Vector(Trait))
    (paramTypes :+ returnType).foreach(fType.addGenericModifier)
    addApplyMethod(fType, paramTypes, returnType)
    fType
  }

  private def addApplyMethod(tpe: ConcreteType, paramTypes: Seq[GenericModifier], returnType: GenericModifier) = {
    val params = paramTypes.zipWithIndex.map(p => typeToParam(p._1, p._2))
    val apply: Method = Method(ctx, "apply", returnType, Vector(Abstract), params)
    tpe.addMethod(apply)
  }

  override def unitType: TType = unit

  override def booleanType: TType = boolean

  override def nullType: TType = nullTpe

  private implicit def typeToParam(tpe: TType): Param = Param(ctx, tpe, "arg0")

  private def typeToParam(tpe: TType, argNum: Int): Param = Param(ctx, tpe, "arg" + argNum)
}
