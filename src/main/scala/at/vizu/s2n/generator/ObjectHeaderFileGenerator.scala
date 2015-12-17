package at.vizu.s2n.generator

import at.vizu.s2n.generator.handles.MethodDefinitionHandle
import at.vizu.s2n.types.result.{ImportStmt, ObjectImplementation}
import at.vizu.s2n.types.symbol._

/**
  * Phil on 12.11.15.
  */
class ObjectHeaderFileGenerator(_baseTypes: BaseTypes, _packageName: String,
                                _imports: Seq[ImportStmt], implementation: ObjectImplementation) extends HeaderFileGenerator {

  override protected def selfType: TType = implementation.tpe

  override protected def baseTypes: BaseTypes = _baseTypes

  override protected def packageName: String = _packageName

  override protected def imports: Seq[ImportStmt] = _imports

  override protected def generateProtectedSection(members: Seq[Member]): String = {
    val tpeName = selfType.simpleName
    val protectedMember =
      s"""
         |  $tpeName();                           // Don't implement
         |  $tpeName($tpeName const &);              // Don't implement
         |  void operator=($tpeName const &);        // Don't implement
     """.stripMargin
    super.generateProtectedSection(members) + protectedMember
  }

  override protected def generatePublicSection(members: Seq[Member]): String = {
    val staticPtr = s"static ${GeneratorUtils.generateSmartPtr(baseTypes, selfType)}"
    val getInstance: String =
      s"""
         |
         |  $staticPtr getInstance() {
         |    $staticPtr instance = std::make_shared<${GeneratorUtils.getCppTypeName(baseTypes, selfType)}>();
         |    return instance;
         |  }""".stripMargin
    super.generatePublicSection(members) + getInstance
  }

  override protected def groupMember(): Map[String, Seq[Member]] = {
    val methodDefinitions = getHandlesSeq(classOf[MethodDefinitionHandle]).map(_.method)
    val member: Seq[Member] = selfType.methods.filter(!_.constructor) ++ selfType.fields ++ methodDefinitions
    member.groupBy(_.visibility) + ("protected" -> Vector())
  }
}
