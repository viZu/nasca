package at.vizu.s2n.types

/**
 * Phil on 06.11.15.
 */
trait ReflectTypeSystemComponent extends TypeComponent {

  import com.softwaremill.macwire._

  lazy val typeSystemInitializer = wire[TypeSystemInitializerImpl]
  lazy val typeChecker = wire[ReflectTypeChecker]
  lazy val typeSystem = wire[BaseTypeSystem]

}
