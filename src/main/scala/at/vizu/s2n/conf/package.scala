package at.vizu.s2n

import at.vizu.s2n.generator.GeneratorContext

/**
  * Phil on 04.12.15.
  */
package object conf {

  implicit def strToHandle(ctx: GeneratorContext): (String, Seq[String]) => GeneratorContext = (varName, params) => ctx

}
