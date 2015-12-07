package at.vizu.s2n.types.symbol

import org.scalatest._

/**
  * Phil on 05.11.15.
  */
class MethodSpec extends FlatSpec with Matchers {

  val ctx = Context("", 0)
  val superType = ConcreteType(ctx, "SuperType", "test")
  val param1 = ConcreteType(ctx, "Param", "test")
  param1.addParent(superType)
  val param2 = ConcreteType(ctx, "AnotherParam", "test")

  "Method.checkArgs" should "return true if correct argument types are presented" in {
    val m = createMethod()
    m.checkArgs(Seq(superType, param2)) should be(true)
  }

  "Method.checkArgs" should "return false if unsatisfied argument types are presented" in {
    val m = createMethod()
    m.checkArgs(Seq(param2, param1)) should be(false)
  }

  "Method.checkArgs" should "return false if a wrong number of argument types are presented" in {
    val m = createMethod()
    m.checkArgs(Seq(param1)) should be(false)
  }

  "Method.checkArgsSuperType" should "return true if correct argument types are presented" in {
    val m = createMethod()
    m.checkArgsSuperType(Seq(superType, param2)) should be(true)
  }

  "Method.checkArgsSuperType" should "return true if correct argument types are presented and one is a subtype of the needed class" in {
    val m = createMethod()
    m.checkArgsSuperType(Seq(param1, param2)) should be(true)
  }

  "Method.checkArgsSuperType" should "return false if a wrong number of argument types are presented" in {
    val m = createMethod()
    m.checkArgsSuperType(Seq(param1)) should be(false)
  }

  "Method.checkArgsSuperType" should "return false if unsatisfied argument types are presented" in {
    val m = createMethod()
    m.checkArgsSuperType(Seq(param2, param1)) should be(false)
  }


  private def createMethod(): Method = {
    val returnType = ConcreteType(ctx, "Return", "test")
    val params: Seq[Param] = Seq(superType, param2).map(Param(ctx, _, "a"))
    Method(ctx, "name", returnType, Seq(), params)
  }
}
