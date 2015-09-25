package at.vizu.s2n.environment

import at.vizu.s2n.args.Arguments

/**
 * Phil on 25.09.15.
 */
object Environment {

  def apply(args: Arguments) = args.env match {
    case "c++" => CppEnvironmentRegistry.environment
    case _ => CppEnvironmentRegistry.environment
  }

}
