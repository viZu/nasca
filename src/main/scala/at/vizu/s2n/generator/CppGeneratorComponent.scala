package at.vizu.s2n.generator

import at.vizu.s2n.types.TypeComponent

/**
 * Phil on 06.11.15.
 */
trait CppGeneratorComponent extends TypeComponent {

  import com.softwaremill.macwire._

  lazy val generator = wire[CppGenerator]

}
