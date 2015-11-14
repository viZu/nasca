package at.vizu.s2n.types

import at.vizu.s2n.types.symbol.ScalaScopeInitializer
import com.softwaremill.macwire._

/**
  * Phil on 12.11.15.
  */
trait TypeComponent {

  lazy val scopeInitializer = wire[ScalaScopeInitializer]

}
