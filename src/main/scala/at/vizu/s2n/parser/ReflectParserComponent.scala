package at.vizu.s2n.parser

/**
 * Phil on 06.11.15.
 */
trait ReflectParserComponent {

  import com.softwaremill.macwire._

  lazy val parser = wire[ReflectParser]

}
