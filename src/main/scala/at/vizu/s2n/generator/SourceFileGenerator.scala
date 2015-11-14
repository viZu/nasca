package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments

/**
  * Phil on 12.11.15.
  */
trait SourceFileGenerator {

  def generateSourceFiles(args: Arguments): Unit

}
