package at.vizu.s2n.log

import at.vizu.s2n.args.Arguments

/**
  * Phil on 25.12.15.
  */
case class LogOptions(logLevel: LogLevel = Warn, stdOut: Option[String] = None, stdErr: Option[String] = None) {

}

object LogOptions {
  def apply(arguments: Arguments): LogOptions = LogOptions(arguments.logLevel, arguments.stdout, arguments.stderr)
}
