package at.vizu.s2n.generator

/**
 * Phil on 06.11.15.
 */
trait CppGeneratorComponent {

  import com.softwaremill.macwire._

  lazy val generator = wire[CppGenerator]

}
