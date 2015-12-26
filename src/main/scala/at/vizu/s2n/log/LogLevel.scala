package at.vizu.s2n.log

import com.typesafe.scalalogging.LazyLogging

/**
  * Phil on 25.12.15.
  */
trait LogLevel {
  val logPriority: Int

  def >=(other: LogLevel) = logPriority >= other.logPriority
}

object LogLevel extends LazyLogging {
  def apply(str: String) = str.toLowerCase match {
    case "trace" => Trace
    case "debug" => Debug
    case "info" => Info
    case "warn" => Warn
    case "error" => Error
    case _ =>
      logger.warn(s"Log level $str not known. Using warn as log level.")
      Warn
  }
}

object Trace extends LogLevel {
  override val logPriority: Int = 0

  override def toString: String = "Trace"

}

object Debug extends LogLevel {
  override val logPriority: Int = 1

  override def toString: String = "Debug"
}

object Info extends LogLevel {
  override val logPriority: Int = 2

  override def toString: String = "Info"
}

object Warn extends LogLevel {
  override val logPriority: Int = 3

  override def toString: String = "Warn"
}

object Error extends LogLevel {
  override val logPriority: Int = 4

  override def toString: String = "Error"
}
