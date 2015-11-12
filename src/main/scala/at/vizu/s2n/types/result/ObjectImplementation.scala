package at.vizu.s2n.types.result

import at.vizu.s2n.generator.GeneratorUtils
import at.vizu.s2n.types.symbol.{Modifiable, TScope, TType}

import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
case class ObjectImplementation(module: ModuleDef, tpe: TType) extends ReflectImplementation {

  override def generateSource(scope: TScope, packageName: String, imports: Seq[ImportStmt]): (String, String) = {
    ("", generateBody(module.impl.body))
  }

  override def thisType: TType = tpe

  override protected def generatePublicSection(members: Seq[Modifiable]): String = {
    val staticPtr = s"static ${GeneratorUtils.generateSmartPtr(tpe)}"
    val getInstance: String =
      s"""$staticPtr getInstance() {
         |  $staticPtr instance = std::make_shared<${GeneratorUtils.getCppTypeName(tpe)}>();
         |  return instance;
         |}
         |""".stripMargin
    getInstance + super.generatePublicSection(members)
  }
}
