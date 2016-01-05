package at.vizu.s2n.packaging

import at.vizu.s2n.lib.CppLibraryComponent

/**
  * Phil on 05.01.16.
  */
trait PackagingComponent extends CppLibraryComponent {

  import com.softwaremill.macwire._

  lazy val packager = wire[PackagerImpl]

}
