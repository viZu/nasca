package at.vizu.s2n.environment

import at.vizu.s2n.parser.ReflectParserComponent
import at.vizu.s2n.types.CppTypeSystemBuilderComponent

/**
 * Phil on 25.09.15.
 */
object CppEnvironmentRegistry extends ReflectParserComponent
with CppTypeSystemBuilderComponent
with CppEnvironmentComponent {

  val parser = new ReflectParser
  val typeSystemBuilder = new CppTypeSystemBuilder

  val environment = new CppEnvironment

}
