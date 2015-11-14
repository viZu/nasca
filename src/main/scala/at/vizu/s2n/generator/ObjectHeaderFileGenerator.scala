package at.vizu.s2n.generator

import at.vizu.s2n.types.result.{ImportStmt, ObjectImplementation}
import at.vizu.s2n.types.symbol.{BaseTypes, Modifiable, TType}

/**
  * Phil on 12.11.15.
  */
class ObjectHeaderFileGenerator(_baseTypes: BaseTypes, _packageName: String,
                                _imports: Seq[ImportStmt], implementation: ObjectImplementation) extends HeaderFileGenerator {

  override protected def selfType: TType = implementation.tpe

  override protected def baseTypes: BaseTypes = _baseTypes

  override protected def packageName: String = _packageName

  override protected def imports: Seq[ImportStmt] = _imports

  override protected def generatePublicSection(members: Seq[Modifiable]): String = {
    val staticPtr = s"static ${GeneratorUtils.generateSmartPtr(selfType)}"
    val getInstance: String =
      s"""$staticPtr getInstance() {
         |  $staticPtr instance = std::make_shared<${GeneratorUtils.getCppTypeName(baseTypes, selfType)}>();
         |  return instance;
         |}
         |""".stripMargin
    getInstance + super.generatePublicSection(members)
  }

}
