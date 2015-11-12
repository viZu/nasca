package at.vizu.s2n.types.symbol

/**
 * Phil on 16.10.15.
 */
class ScalaScopeInitializer extends ScopeInitializer with BaseTypes {

  lazy val ctx: Context = Context("", 0)
  lazy val any = new TType(simpleName = "Any", pkg = "scala")
  lazy val anyVal = new TType(simpleName = "AnyVal", pkg = "scala")
  lazy val anyRef = new TType(simpleName = "AnyRef", pkg = "scala")
  lazy val string = new TType(simpleName = "String", pkg = "scala", mods = Seq(Trait))
  lazy val numeric = new TType(simpleName = "NumericPrimitive", pkg = "scala", mods = Seq(Trait))
  lazy val unit = new TType(simpleName = "Unit", pkg = "scala", mods = Seq(Trait))
  lazy val boolean = new TType(simpleName = "Boolean", pkg = "scala", mods = Seq(Trait))
  lazy val byte = new TType(simpleName = "Byte", pkg = "scala", mods = Seq(Trait))
  lazy val short = new TType(simpleName = "Short", pkg = "scala", mods = Seq(Trait))
  lazy val char = new TType(simpleName = "Char", pkg = "scala", mods = Seq(Trait))
  lazy val int = new TType(simpleName = "Int", pkg = "scala", mods = Seq(Trait))
  lazy val long = new TType(simpleName = "Long", pkg = "scala", mods = Seq(Trait))
  lazy val float = new TType(simpleName = "Float", pkg = "scala", mods = Seq(Trait))
  lazy val double = new TType(simpleName = "Double", pkg = "scala", mods = Seq(Trait))
  lazy val nullTpe = new TType(simpleName = "Null", pkg = "scala", mods = Seq(Trait))

  lazy val primitives = Set(boolean, byte, short, char, int, long, float, double)

  override def initScope: TScope = {
    val scope: TScope = new TScope()
    val a: TType = initAny()
    val ar: TType = initAnyRef()
    val av: TType = initAnyVal()

    val anys = Seq(a, ar, av)
    val str = initString()
    val numericPrimitive = initNumericPrimitive()
    val pris = initPrimitives()
    val nullT = initNull()
    val allTypes = anys ++ pris :+ numericPrimitive :+ str :+ nullT

    scope.addAllClasses(allTypes)

    allTypes foreach (t => {
      scope.addTypeAlias(t.simpleName, t.name)
    })

    scope
  }

  private def initAny() = {
    //TODO Context
    any.addMethod(Method(ctx, "$bang$eq", boolean, Seq(Final), Seq(Param(ctx, any, "arg0"))))
    any.addMethod(Method(ctx, "$eq$eq", boolean, Seq(Final), Seq(Param(ctx, any, "arg0"))))
    any.addMethod(Method(ctx, "equals", boolean, Seq(), Seq(Param(ctx, any, "arg0"))))
    any.addMethod(Method(ctx, "hashCode", int, Seq(), Seq()))
    //any.addMethod(Method(ctx, "toString", any, Seq(), Seq(Param(any, "arg0"))))
    any
  }

  private def initAnyVal() = {
    //TODO Context
    anyVal.parents = Seq(any)
    anyVal
  }

  private def initAnyRef() = {
    //TODO Context
    anyRef.parents = Seq(any)
    anyRef.addMethod(Method(ctx, "eq", boolean, Seq(Final), Seq(Param(ctx, anyRef, "arg0"))))
    anyRef.addMethod(Method(ctx, "ne", boolean, Seq(Final), Seq(Param(ctx, anyRef, "arg0"))))
    anyRef
  }

  private def initString() = {
    string.parents = Seq(anyRef)
    string.addMethod(Method(ctx, "$times", string, Seq(Abstract), Seq(Param(ctx, string, "x"))))
    string.addMethod(Method(ctx, "$plus", string, Seq(Abstract), Seq(Param(ctx, string, "x"))))
    string
  }

  private def initNumericPrimitive() = {
    numeric.parents = Seq()
    numeric.addMethod(Method(ctx, "$less", boolean, Seq(Abstract), Seq(Param(ctx, numeric, "x"))))
    numeric.addMethod(Method(ctx, "$less$eq", boolean, Seq(Abstract), Seq(Param(ctx, numeric, "x"))))
    numeric.addMethod(Method(ctx, "$greater", boolean, Seq(Abstract), Seq(Param(ctx, numeric, "x"))))
    numeric.addMethod(Method(ctx, "$greater$eq", boolean, Seq(Abstract), Seq(Param(ctx, numeric, "x"))))
    numeric.addMethod(Method(ctx, "toByte", byte, Seq(Abstract), Seq()))
    numeric.addMethod(Method(ctx, "toShort", short, Seq(Abstract), Seq()))
    numeric.addMethod(Method(ctx, "toChar", char, Seq(Abstract), Seq()))
    numeric.addMethod(Method(ctx, "toInt", int, Seq(Abstract), Seq()))
    numeric.addMethod(Method(ctx, "toLong", long, Seq(Abstract), Seq()))
    numeric.addMethod(Method(ctx, "toFloat", float, Seq(Abstract), Seq()))
    numeric.addMethod(Method(ctx, "toDouble", double, Seq(Abstract), Seq()))
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
    Seq(d, f, l, i, c, s, by, b, u)
  }

  private def initUnit() = {
    //TODO Context
    unit.parents = Seq(anyVal)
    unit
  }

  private def initBoolean() = {
    //TODO Context
    boolean.parents = Seq(anyVal)
    boolean.addMethod(Method(ctx, "$less", boolean, Seq(Abstract), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$less$eq", boolean, Seq(Abstract), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$greater", boolean, Seq(Abstract), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$greater$eq", boolean, Seq(Abstract), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$bang$eq", boolean, Seq(Abstract, Override), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$eq$eq", boolean, Seq(Abstract, Override), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$amp$amp", boolean, Seq(Abstract), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "$bar$bar", boolean, Seq(Abstract), Seq(Param(ctx, boolean, "x"))))
    boolean.addMethod(Method(ctx, "unary_$bang", boolean, Seq(Abstract), Seq()))
    boolean
  }

  private def initByte() = {
    //TODO Context
    byte.parents = Seq(short)
    byte.addMethod(Method(ctx, "unary_$minus", int, Seq(Abstract, Override), Seq()))
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
    short.parents = Seq(int)
    short.addMethod(Method(ctx, "unary_$minus", int, Seq(Abstract, Override), Seq()))
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
    char.parents = Seq(int)
    char.addMethod(Method(ctx, "unary_$minus", int, Seq(Abstract, Override), Seq()))
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
    int.parents = Seq(long)
    int.addMethod(Method(ctx, "unary_$minus", int, Seq(Abstract, Override), Seq()))
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
    long.parents = Seq(float)
    long.addMethod(Method(ctx, "unary_$minus", long, Seq(Abstract, Override), Seq()))
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
    float.parents = Seq(double)
    float.addMethod(Method(ctx, "unary_$minus", float, Seq(Abstract, Override), Seq()))
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
    double.parents = Seq(anyVal, numeric)
    double.addMethod(Method(ctx, "unary_$minus", double, Seq(Abstract, Override), Seq()))
    calcMethods(double, double, double)
    calcMethods(double, float, double)
    calcMethods(double, long, double)
    calcMethods(double, int, double)
    calcMethods(double, char, double)
    calcMethods(double, short, double)
    calcMethods(double, byte, double)
    double
  }

  private def calcMethods(appendTo: TType, tpe: TType, retTpe: TType) = {
    appendTo.addMethod(Method(ctx, "$times", retTpe, Seq(Abstract, Override), Seq(Param(ctx, tpe, "x"))))
    appendTo.addMethod(Method(ctx, "$plus", retTpe, Seq(Abstract, Override), Seq(Param(ctx, tpe, "x"))))
    appendTo.addMethod(Method(ctx, "$div", retTpe, Seq(Abstract, Override), Seq(Param(ctx, tpe, "x"))))
    appendTo.addMethod(Method(ctx, "$minus", retTpe, Seq(Abstract, Override), Seq(Param(ctx, tpe, "x"))))
    appendTo.addMethod(Method(ctx, "$percent", retTpe, Seq(Abstract, Override), Seq(Param(ctx, tpe, "x"))))
  }

  private def initNull() = {
    nullTpe.parents = Seq(string)
    nullTpe
  }

  override def unitType: TType = unit

  override def booleanType: TType = boolean

  override def nullType: TType = nullTpe
}
