package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.generator.handles.GeneratorHandle

/**
  * Phil on 12.11.15.
  */
trait SourceFileGenerator {

  def generateSourceFile(args: Arguments): Seq[GeneratorHandle]

}
