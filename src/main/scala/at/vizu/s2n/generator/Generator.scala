package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.result.ScalaFileWrapper

/**
 * Phil on 06.11.15.
 */
trait Generator {

  def generateCode(args: Arguments, fileContents: Seq[ScalaFileWrapper]): Unit

}
