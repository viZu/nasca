package at.vizu.s2n.generator

import at.vizu.s2n.types.result.ImportStmt
import at.vizu.s2n.types.symbol._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Phil on 11.11.15.
  */
class GeneratorUtilsSpec extends FlatSpec with Matchers {

  val baseTypes = new ScalaScopeInitializer
  val sharedPtr = "std::shared_ptr"
  val ctx = Context("", 0)
  val tpeWithOutPkg = TType(Context("test", 0), "Type")
  val tpeWithPkg = TType(Context("test", 0), "Type", "test")
  val tpeWithNestedPkg = TType(Context("test", 0), "Type", "test.a.b.c")

  "GeneratorUtils.getCppTypeName" should "return correct C++ typename with no package" in {
    GeneratorUtils.getCppTypeName("", "Type") should be("Type")
  }

  "GeneratorUtils.getCppTypeName" should "return correct C++ typename with package" in {
    GeneratorUtils.getCppTypeName("test", "Type") should be("test::Type")
  }

  "GeneratorUtils.getCppTypeName" should "return correct C++ typename with nested package" in {
    GeneratorUtils.getCppTypeName("test.a.b.c", "Type") should be("test_a_b_c::Type")
  }

  "GeneratorUtils.getCppTypeName" should "return correct C++ typename for type with no package" in {
    GeneratorUtils.getCppTypeName(tpeWithOutPkg) should be("Type")
  }

  "GeneratorUtils.getCppTypeName" should "return correct C++ typename for type with package" in {
    GeneratorUtils.getCppTypeName(tpeWithPkg) should be("test::Type")
  }

  "GeneratorUtils.getCppTypeName" should "return correct C++ typename for type with nested package" in {
    GeneratorUtils.getCppTypeName(tpeWithNestedPkg) should be("test_a_b_c::Type")
  }

  "GeneratorUtils.generateSmartPtr" should "return correct C++ smart pointer with no package" in {
    GeneratorUtils.generateSmartPtr("", "Type") should be(s"${sharedPtr}<Type>")
  }

  "GeneratorUtils.generateSmartPtr" should "return correct C++ smart pointer with package" in {
    GeneratorUtils.generateSmartPtr("test", "Type") should be(s"${sharedPtr}<test::Type>")
  }

  "GeneratorUtils.generateSmartPtr" should "return correct C++ smart pointer with nested package" in {
    GeneratorUtils.generateSmartPtr("test.a.b.c", "Type") should be(s"${sharedPtr}<test_a_b_c::Type>")
  }

  "GeneratorUtils.generateSmartPtr" should "return correct C++ smart pointer for type with no package" in {
    GeneratorUtils.generateSmartPtr(tpeWithOutPkg) should be(s"${sharedPtr}<Type>")
  }

  "GeneratorUtils.generateSmartPtr" should "return correct C++ smart pointer for type with package" in {
    GeneratorUtils.generateSmartPtr(tpeWithPkg) should be(s"${sharedPtr}<test::Type>")
  }

  "GeneratorUtils.generateSmartPtr" should "return correct C++ smart pointer for type with nested package" in {
    GeneratorUtils.generateSmartPtr(tpeWithNestedPkg) should be(s"${sharedPtr}<test_a_b_c::Type>")
  }

  "GeneratorUtils.generateFieldDefinition" should "return C++ field for scala field with no package" in {
    val field = Field(ctx, Seq(), "aField", tpeWithOutPkg)
    GeneratorUtils.generateFieldDefinition(field) should be(s"${sharedPtr}<Type> aField;")
  }

  "GeneratorUtils.generateFieldDefinition" should "return C++ field for scala field with package" in {
    val field = Field(ctx, Seq(), "aField", tpeWithPkg)
    GeneratorUtils.generateFieldDefinition(field) should be(s"${sharedPtr}<test::Type> aField;")
  }

  "GeneratorUtils.generateFieldDefinition" should "return C++ field for scala field with nested package" in {
    val field = Field(ctx, Seq(), "aField", tpeWithNestedPkg)
    GeneratorUtils.generateFieldDefinition(field) should be(s"${sharedPtr}<test_a_b_c::Type> aField;")
  }

  "GeneratorUtils.generateMethodDefinition" should "return C++ method for scala method with no param" in {
    val method = Method(ctx, "aMethod", tpeWithOutPkg, Seq(), Seq())
    GeneratorUtils.generateMethodDefinition(method) should be(s"${sharedPtr}<Type> aMethod();")
  }

  "GeneratorUtils.generateMethodDefinition" should "return C++ method for scala method with one param" in {
    val method = Method(ctx, "aMethod", tpeWithOutPkg, Seq(), Seq(Param(ctx, tpeWithPkg, "a")))
    GeneratorUtils.generateMethodDefinition(method) should be(s"${sharedPtr}<Type> aMethod(${sharedPtr}<test::Type>);")
  }

  "GeneratorUtils.generateMethodDefinition" should "return C++ method for scala method with two param" in {
    val method = Method(ctx, "aMethod", tpeWithPkg, Seq(), Seq(Param(ctx, tpeWithOutPkg, "a"), Param(ctx, tpeWithNestedPkg, "b")))
    GeneratorUtils.generateMethodDefinition(method) should be(s"${sharedPtr}<test::Type> aMethod(${sharedPtr}<Type>, ${sharedPtr}<test_a_b_c::Type>);")
  }

  "GeneratorUtils.generateIncludes" should "return empty C++ Includes for no scala Imports" in {
    GeneratorUtils.generateIncludes(Seq()) should be("")
  }

  "GeneratorUtils.generateIncludes" should "return correct C++ Include for a single scala Import" in {
    GeneratorUtils.generateIncludes(Seq(ImportStmt("", "Type", ""))) should be(
      """#include "Type.h"
        |
        | """.stripMargin)
  }

  "GeneratorUtils.generateIncludes" should "return correct C++ Includes for multiple scala Imports" in {
    GeneratorUtils.generateIncludes(Seq(ImportStmt("", "Type", ""), ImportStmt("", "Type2", ""))) should be(
      """#include "Type.h"
        |#include "Type2.h"
        |
        | """.stripMargin)
  }

  "GeneratorUtils.getPrimitiveName" should "return void for scala.Unit" in {
    GeneratorUtils.getPrimitiveName(baseTypes.unit) should be("void")
  }

  "GeneratorUtils.getPrimitiveName" should "return std::string for scala.String" in {
    GeneratorUtils.getPrimitiveName(baseTypes.string) should be("std::string")
  }

  "GeneratorUtils.getPrimitiveName" should "return bool for scala.Boolean" in {
    GeneratorUtils.getPrimitiveName(baseTypes.boolean) should be("bool")
  }

  "GeneratorUtils.getPrimitiveName" should "return lowercase typename for all other scala primitives" in {
    GeneratorUtils.getPrimitiveName(baseTypes.long) should be("long")
    GeneratorUtils.getPrimitiveName(baseTypes.int) should be("int")
    GeneratorUtils.getPrimitiveName(baseTypes.double) should be("double")
    GeneratorUtils.getPrimitiveName(baseTypes.char) should be("char")
  }
}
