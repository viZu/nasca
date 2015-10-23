package at.vizu.s2n.types.symbol

/**
 * Phil on 16.10.15.
 */
trait ScalaScopeInitializer extends ScopeInitializer {

  override def initScope: Scope = {
    val scope: Scope = new Scope()
    val a: Type = any()
    val ar: Type = anyRef(a)
    val av: Type = anyVal(a)

    val anys = Seq(a, ar, av)
    val pris = primitives(av)
    val allTypes = anys ++ pris
    scope.addAllClasses(allTypes)

    allTypes foreach (t => {
      scope.addTypeAlias(t.name, t.fullClassName)
    })

    scope
  }

  private def any() = {
    //TODO Context
    val any = new Type(name = "Any", pkg = "scala")
    any
  }

  private def anyVal(any: Type) = {
    //TODO Context
    val anyVal = new Type(name = "AnyVal", pkg = "scala")
    anyVal.parents = Seq(any)
    anyVal
  }

  private def anyRef(any: Type) = {
    //TODO Context
    val anyRef = new Type(name = "AnyRef", pkg = "scala")
    anyRef.parents = Seq(any)
    anyRef
  }

  private def primitives(av: Type): Seq[Type] = {
    primitiveAcc(av, unit, boolean, byte, short, char, int, long, float, double)
  }

  private def primitiveAcc(anyVal: Type, funs: (Type => Type)*): Seq[Type] = {
    funs.map(f => f(anyVal))
  }

  private def unit(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Unit", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def boolean(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Boolean", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def byte(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Byte", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def short(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Short", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def char(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Char", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def int(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Int", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def long(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Long", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def float(anyVal: Type) = {
    val pri = new Type(name = "Float", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }

  private def double(anyVal: Type) = {
    //TODO Context
    val pri = new Type(name = "Double", pkg = "scala")
    pri.parents = Seq(anyVal)
    pri
  }
}
