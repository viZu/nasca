package at.vizu.s2n.types.symbol

import org.scalatest._

/**
  * Phil on 05.11.15.
  */
class FieldSpec extends FlatSpec with Matchers {

  val tpe = ConcreteType(Context("test", 0), "FieldType", "test")
  val field = Field(Context("test", 0), Vector(Private, Abstract), "field", tpe)
  val mutableField = Field(Context("test", 0), Vector(Private, Mutable), "field", tpe)

  "Field.asIdentifier" should "return correct Identifier" in {
    field.asIdentifier should be(Identifier(Context("test", 0), "field", tpe, mutable = false, fromField = true))
  }

  "a mutable Field.asIdentifier" should "return correct mutable Identifier" in {
    mutableField.asIdentifier should be(Identifier(Context("test", 0), "field", tpe, mutable = true, fromField = true))
  }
}
