package at.vizu.s2n.environment

import at.vizu.s2n.args.Arguments

/**
 * Phil on 06.11.15.
 */
trait Environment {

  def compile(arguments: Arguments): Unit

}
