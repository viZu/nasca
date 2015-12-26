package at.vizu.s2n.log

import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import org.slf4j.{Logger => SLF4JLogger, LoggerFactory}

/**
  * Phil on 25.12.15.
  */
object Log {

  def init(options: LogOptions): Unit = {
    val log = LoggerFactory.getLogger(SLF4JLogger.ROOT_LOGGER_NAME).asInstanceOf[LogbackLogger]
    log.setLevel(getLevel(options.logLevel))
  }

  def getLevel(logLevel: LogLevel) = logLevel match {
    case Trace => Level.TRACE
    case Debug => Level.DEBUG
    case Info => Level.INFO
    case Warn => Level.WARN
    case Error => Level.ERROR
  }

}
