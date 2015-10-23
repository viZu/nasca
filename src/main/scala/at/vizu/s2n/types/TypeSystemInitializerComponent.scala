package at.vizu.s2n.types

/**
 * Phil on 23.10.15.
 */
trait TypeSystemInitializerComponent {

  val typeSystemInitializer: TypeSystemInitializer

  trait TypeSystemInitializer {
    def initTypeSystem()
  }

}
