package at.vizu.s2n.environment

import at.vizu.s2n.parser.ReflectParserComponent

/**
 * Phil on 25.09.15.
 */
object CppEnvironmentRegistry extends ReflectParserComponent with CppEnvironmentComponent {

  val parser = new ReflectParser

  val environment = new CppEnvironment

}
