package at.vizu.s2n.ext.compiler

/**
  * Phil on 03.12.15.
  */
trait CppCompilerComponent {

  import com.softwaremill.macwire._

  lazy val extGenerator = wire[CppCompiler]

}
