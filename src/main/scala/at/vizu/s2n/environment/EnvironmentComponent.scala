package at.vizu.s2n.environment

import at.vizu.s2n.args.Arguments

/**
 * Phil on 25.09.15.
 */
trait EnvironmentComponent {

  trait Environment {
    def compile(arguments: Arguments): Unit
  }

}
