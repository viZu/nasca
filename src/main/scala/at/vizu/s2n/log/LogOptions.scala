package at.vizu.s2n.log

import java.io.PrintStream

import at.vizu.s2n.args.Arguments

/**
  * Phil on 25.12.15.
  */
case class LogOptions(logLevel: LogLevel = Warn, stdOut: PrintStream = System.out, stdErr: PrintStream = System.err) {

}

object LogOptions {
  def apply(arguments: Arguments): LogOptions = LogOptions(logLevel = arguments.logLevel)
}
