package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.result.ScalaFileWrapper
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 06.11.15.
 */
trait Generator {

  def generateCode(args: Arguments, scope: TScope, fileContents: Seq[ScalaFileWrapper]): Unit

}
