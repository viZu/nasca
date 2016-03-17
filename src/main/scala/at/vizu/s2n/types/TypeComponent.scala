package at.vizu.s2n.types

import at.vizu.s2n.lib.CppLibraryComponent
import at.vizu.s2n.types.symbol.ScalaSymbolTableInitializer
import com.softwaremill.macwire._

/**
  * Phil on 12.11.15.
  */
trait TypeComponent extends CppLibraryComponent {

  lazy val scopeInitializer = wire[ScalaSymbolTableInitializer]

}
