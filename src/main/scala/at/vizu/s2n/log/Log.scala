package at.vizu.s2n.log

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.filter.ThresholdFilter
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger => LogbackLogger, LoggerContext}
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.filter.Filter
import org.slf4j.{Logger => SLF4JLogger, LoggerFactory}

/**
  * Phil on 25.12.15.
  */
object Log {

  val Pattern = "[%level] %msg%n"
  val StdOutAppender = "STDOUT"
  val StdErrAppender = "STDERR"
  val FileOutAppender = "FILEOUT"
  val FileErrAppender = "FILEERR"

  lazy val rootLogger = LoggerFactory.getLogger(SLF4JLogger.ROOT_LOGGER_NAME).asInstanceOf[LogbackLogger]
  lazy val context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  def init(options: LogOptions): Unit = {
    rootLogger.setLevel(getLevel(options.logLevel))
    initStdErr(options.stdErr)
    initStdOut(options.stdOut)
  }

  private def getLevel(logLevel: LogLevel) = logLevel match {
    case Trace => Level.TRACE
    case Debug => Level.DEBUG
    case Info => Level.INFO
    case Warn => Level.WARN
    case Error => Level.ERROR
  }

  private def initStdErr(stdErrOpt: Option[String]) = {
    stdErrOpt.foreach(stdErr => {
      stopAppender(StdErrAppender)
      val filter = new ThresholdFilter
      filter.setLevel("ERROR")
      createFileAppender(FileErrAppender, stdErr, filter)
    })
  }

  private def initStdOut(stdOutOpt: Option[String]) = {
    stdOutOpt.foreach(stdOut => {
      stopAppender(StdOutAppender)
      val filter: MaxLevelFilter = new MaxLevelFilter
      filter.setMaxLevel(Level.WARN)
      createFileAppender(FileOutAppender, stdOut, filter)
    })
  }

  private def stopAppender(name: String) = {
    val appender = rootLogger.getAppender(name)
    appender.stop()
  }

  private def createFileAppender(tag: String, file: String, filter: Filter[ILoggingEvent]) = {
    var appender = rootLogger.getAppender(tag).asInstanceOf[FileAppender[ILoggingEvent]]
    if (appender != null) configureFileAppender(appender, file, filter)
    else {
      appender = new FileAppender[ILoggingEvent]
      appender.setName(tag)
      configureFileAppender(appender, file, filter)
    }

  }

  private def configureFileAppender(appender: FileAppender[ILoggingEvent], file: String, filter: Filter[ILoggingEvent]) = {

    val ple: PatternLayoutEncoder = new PatternLayoutEncoder
    ple.setPattern(Pattern)
    ple.setContext(context)
    ple.start()

    appender.setContext(context)
    appender.setEncoder(ple)
    appender.setFile(file)
    filter.start()
    appender.addFilter(filter)
    appender.start()
    rootLogger.addAppender(appender)
    appender
  }
}
