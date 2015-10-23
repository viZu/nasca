package at.vizu.s2n.generator

/**
 * Phil on 08.10.15.
 */
trait GeneratorComponent {

  val generator: Generator

  trait Generator {
    def generateCode()
  }

}
