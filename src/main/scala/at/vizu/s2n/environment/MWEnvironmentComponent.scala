package at.vizu.s2n.environment

import at.vizu.s2n.generator.CppGenerator
import at.vizu.s2n.parser.ReflectParser
import at.vizu.s2n.types.symbol.ScalaScopeInitializer
import at.vizu.s2n.types.{BaseTypeSystem, TypeSystemInitializerImpl}

/**
 * Phil on 06.11.15.
 */
object MWEnvironmentComponent {

  import com.softwaremill.macwire._

  lazy val parser = wire[ReflectParser]

  lazy val scopeInitializer = wire[ScalaScopeInitializer]
  lazy val typeSystemInitializer = wire[TypeSystemInitializerImpl]
  lazy val typeSystem = wire[BaseTypeSystem]

  lazy val generator = wire[CppGenerator]

  lazy val environment = wire[CppEnvironment]

}
