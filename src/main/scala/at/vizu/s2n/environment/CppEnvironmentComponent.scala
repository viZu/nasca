package at.vizu.s2n.environment

import at.vizu.s2n.ext.compiler.CppCompilerComponent
import at.vizu.s2n.generator.CppGeneratorComponent
import at.vizu.s2n.packaging.PackagingComponent
import at.vizu.s2n.parser.ReflectParserComponent
import at.vizu.s2n.types.ReflectTypeSystemComponent

/**
 * Phil on 06.11.15.
 */
object CppEnvironmentComponent
  extends ReflectParserComponent
  with ReflectTypeSystemComponent
  with CppGeneratorComponent
  with CppCompilerComponent
  with PackagingComponent {

  import com.softwaremill.macwire._

  lazy val environment = wire[CppEnvironment]

}
