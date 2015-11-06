package at.vizu.s2n.environment

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.exception.ArgumentException

/**
 * Phil on 25.09.15.
 */
object Environments {

  def apply(args: Arguments) = args.env match {
    case "c++" => MWEnvironmentComponent.environment
    case "" => MWEnvironmentComponent.environment
    case _ => throw new ArgumentException(s"Could not find compiler environment '${args.env}'")
  }

}
