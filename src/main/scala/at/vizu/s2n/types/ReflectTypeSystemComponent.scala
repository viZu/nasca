package at.vizu.s2n.types

import at.vizu.s2n.types.symbol.ScalaScopeInitializer

/**
 * Phil on 06.11.15.
 */
trait ReflectTypeSystemComponent {

  import com.softwaremill.macwire._

  lazy val scopeInitializer = wire[ScalaScopeInitializer]
  lazy val typeSystemInitializer = wire[TypeSystemInitializerImpl]
  lazy val typeChecker = wire[ReflectTypeChecker]
  lazy val typeSystem = wire[BaseTypeSystem]

}
