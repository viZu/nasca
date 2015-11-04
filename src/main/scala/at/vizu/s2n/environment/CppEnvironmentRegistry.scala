package at.vizu.s2n.environment

import at.vizu.s2n.generator.CppGeneratorComponent
import at.vizu.s2n.parser.ReflectParserComponent
import at.vizu.s2n.types.symbol.ScalaScopeInitializer
import at.vizu.s2n.types.{TypeSystemComponentImpl, TypeSystemInitializerComponentImpl}

/**
 * Phil on 25.09.15.
 */
object CppEnvironmentRegistry extends ReflectParserComponent
with TypeSystemComponentImpl
with CppEnvironmentComponent
with CppGeneratorComponent
with TypeSystemInitializerComponentImpl {
  val parser = new ReflectParser
  val typeSystemInitializer = new TypeSystemInitializerImpl with ScalaScopeInitializer
  val typeSystem = new BaseTypeSystem
  val generator = new CppGenerator
  val environment = new CppEnvironment
}
