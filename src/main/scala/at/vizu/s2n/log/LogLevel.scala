package at.vizu.s2n.log

import com.typesafe.scalalogging.LazyLogging

/**
  * Phil on 25.12.15.
  */
sealed trait LogLevel {
  val logPriority: Int

  def >=(other: LogLevel) = logPriority >= other.logPriority
}

case object LogLevel extends LazyLogging {
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

  def values = Set(Trace, Debug, Info, Warn, Error)

  def stringValues = values.map(_.toString.toLowerCase)
}

case object Trace extends LogLevel {
  override val logPriority: Int = 0

  override def toString: String = "Trace"

}

case object Debug extends LogLevel {
  override val logPriority: Int = 1

  override def toString: String = "Debug"
}

case object Info extends LogLevel {
  override val logPriority: Int = 2

  override def toString: String = "Info"
}

case object Warn extends LogLevel {
  override val logPriority: Int = 3

  override def toString: String = "Warn"
}

case object Error extends LogLevel {
  override val logPriority: Int = 4

  override def toString: String = "Error"
}
