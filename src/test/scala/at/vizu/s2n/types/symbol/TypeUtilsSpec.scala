package at.vizu.s2n.types.symbol

import org.scalatest._

/**
  * Phil on 05.11.15.
  */
class TypeUtilsSpec extends FlatSpec with Matchers {

  "TypeUtils.toString" should "return a well formatted comma seperated String for Nameables" in {
    val ctx = Context("", 0)
    val bType = ConcreteType(ctx, "BType")
    val cType = ConcreteType(ctx, "CType", "a")
    val dType = ConcreteType(ctx, "DType", "b")
    val eType = ConcreteType(ctx, "EType", "c")
    val fType = ConcreteType(ctx, "FType", "d")
    TypeUtils.toString(Vector(bType, cType, dType, eType, fType)) should be("BType, a.CType, b.DType, c.EType, d.FType")
  }

  "TypeUtils.toString" should "return an empty String for an empty Seq" in {
    TypeUtils.toString(Vector()) should be("")
  }
}
