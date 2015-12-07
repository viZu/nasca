package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException
import org.scalatest._

/**
  * Phil on 05.11.15.
  */
class TTypeSpec extends FlatSpec with Matchers {

  val ctx = Context("", 0)
  val int = ConcreteType(ctx, "Int", "scala")
  val string = ConcreteType(ctx, "String", "scala")

  val traitType = ConcreteType(ctx, "TraitType", "test")
  val tparams: Seq[Param] = Seq(int, string).map(Param(ctx, _, "_any"))
  private val tMethod: Method = Method(ctx, "tMethod", int, Seq(), tparams)
  traitType.addMethod(tMethod)
  private val tField: Field = Field(ctx, Seq(), "tField", int)
  traitType.addField(tField)

  val superType = ConcreteType(ctx, "SuperType", "test")
  val sparams: Seq[Param] = Seq(string).map(Param(ctx, _, "_any"))
  private val sMethod: Method = Method(ctx, "sMethod", string, Seq(), sparams)
  superType.addMethod(sMethod)
  private val sField: Field = Field(ctx, Seq(), "sField", int)
  superType.addField(sField)
  superType.addParent(traitType)

  val aType = ConcreteType(ctx, "AType", "test")
  val aparams: Seq[Param] = Seq(string, string).map(Param(ctx, _, "_any"))
  private val aMethod: Method = Method(ctx, "aMethod", string, Seq(), aparams)
  aType.addMethod(aMethod)
  private val aaMethod: Method = Method(ctx, "aaMethod", int, Seq(), aparams)
  aType.addMethod(aaMethod)
  private val aField: Field = Field(ctx, Seq(), "aField", int)
  aType.addField(aField)
  aType.addParent(superType)

  val bType = ConcreteType(ctx, "BType")
  val cType = ConcreteType(ctx, "CType", "test")
  val dType = ConcreteType(ctx, "DType", "test")
  val eType = ConcreteType(ctx, "EType", "test")
  val fType = ConcreteType(ctx, "FType", "test")

  initBType()
  initCType()
  initDType()

  "AType findMethod" should "return its method with correct name and correct params" in {
    aType.findMethod(aType, "aMethod", Seq(string, string)) should be(Some(aMethod))
    aType.findMethod(aType, "aaMethod", Seq(string, string)) should be(Some(aaMethod))
  }

  "AType findMethod" should "return no method with incorrect name and/or incorrect params" in {
    aType.findMethod(aType, "bMethod", Seq(string, string)) should be(None)
    aType.findMethod(aType, "aaMethod", Seq(string, int)) should be(None)
  }

  "AType findMethod" should "return super method with correct name and correct params" in {
    aType.findMethod(aType, "sMethod", Seq(string)) should be(Some(sMethod))
    aType.findMethod(aType, "tMethod", Seq(int, string)) should be(Some(tMethod))
  }

  "AType findMethod" should "return no method with incorrect name and incorrect params for super method" in {
    aType.findMethod(aType, "sMethod", Seq(int)) should be(None)
    aType.findMethod(aType, "ttMethod", Seq(int, string)) should be(None)
  }

  "AType findField" should "return field with correct name" in {
    aType.findField(aType, "aField") should be(Some(aField))
  }

  "AType findField" should "return no field with incorrect name" in {
    aType.findField(aType, "bField") should be(None)
  }

  "AType findField" should "return super field with correct name" in {
    aType.findField(aType, "tField") should be(Some(tField))
    aType.findField(aType, "sField") should be(Some(sField))
  }

  "AType hasParent" should "return true for Trait and Supertype" in {
    aType.hasParent(traitType) should be(true)
    aType.hasParent(superType) should be(true)
  }

  "AType hasParent" should "return false for BType" in {
    aType.hasParent(bType) should be(false)
  }

  "AType foreachType" should "be executed three times (AType, Trait and Supertype)" in {
    var i = 0
    aType.foreachType(t => i += 1)
    i should be(3)
  }

  "Trait foreachType" should "be executed once" in {
    var i = 0
    traitType.foreachType(t => i += 1)
    i should be(1)
  }

  "DType foreachType" should "be executed seven times" in {
    var i = 0
    dType.foreachType(t => i += 1)
    i should be(7)
  }

  "AType fullClassname" should "be a concatenated version of name + package" in {
    aType.fullClassName should be(aType.pkg + "." + aType.simpleName)
  }

  "BType fullClassname" should "be only the classname hence it has no package" in {
    bType.fullClassName should be(bType.name)
  }

  "EType addMethod tMethod" should "add the method to the type" in {
    eType.addMethod(tMethod)
    eType.findMethod(aType, tMethod.name, tMethod.params.map(_.tpe)) should be(Some(tMethod))
  }

  "EType addMethod tMethod a second time" should "throw a TypeException" in {
    an[TypeException] should be thrownBy eType.addMethod(tMethod)
  }

  "EType addField tField" should "add the field to the type" in {
    eType.addField(tField)
    eType.findField(eType, tField.name) should be(Some(tField))
  }

  "EType addField tField a second time" should "throw a TypeException" in {
    an[TypeException] should be thrownBy eType.addField(tField)
  }

  "AType validate" should "run without exception" in {
    aType.validate()
  }

  "CType validate" should "throw a TypeException because of parents" in {
    the[TypeException] thrownBy {
      cType.validate()
    } should have message s"Type ${bType.name} needs to be a trait to be mixed in"
  }

  "EType validate" should "throw a TypeException because of abstract method" in {
    eType.addMethod(Method(ctx, "aName", int, Seq(Abstract), Seq()))
    the[TypeException] thrownBy {
      eType.validate()
    } should have message s"Type ${eType.name} must either be abstract or implement abstract member aName"
  }

  "FType validate" should "throw a TypeException because of abstract field" in {
    fType.addField(Field(ctx, Seq(Abstract), "aField", int))
    the[TypeException] thrownBy {
      fType.validate()
    } should have message s"Type ${fType.name} must either be abstract or implement abstract member aField"
  }

  private def initBType(): Unit = {
    val params: Seq[Param] = Seq(int, int).map(Param(ctx, _, "_any"))
    bType.addMethod(Method(ctx, "bMethod", string, Seq(), params))
    bType.addMethod(Method(ctx, "bbMethod", int, Seq(), params))
    bType.addField(Field(ctx, Seq(), "bField", int))
    bType.addParent(superType)
  }

  private def initCType(): Unit = {
    cType.addParent(aType)
    cType.addParent(bType)
  }

  private def initDType(): Unit = {
    val twoType: ConcreteType = ConcreteType(ctx, "2Trait", "test", Seq(Trait))
    twoType.addParent(traitType)
    dType.addParent(twoType)
    val threeType: ConcreteType = ConcreteType(ctx, "3Trait", "test", Seq(Trait))
    threeType.addParent(ConcreteType(ctx, "4Trait", "test", Seq(Trait)))
    dType.addParent(threeType)
    dType.addParent(ConcreteType(ctx, "5Trait", "test", Seq(Trait)))
    dType.addParent(ConcreteType(ctx, "6Trait", "test", Seq(Trait)))

  }

}
