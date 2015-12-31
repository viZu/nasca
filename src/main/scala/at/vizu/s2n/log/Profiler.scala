package at.vizu.s2n.log

import at.vizu.s2n.error.Errors
import com.typesafe.scalalogging.Logger

/**
  * Phil on 26.12.15.
  */
object Profiler {
  def profileFunc[U](logger: Logger, tag: String, execution: () => U, logLevelFinish: LogLevel = Info): U = {
    profileInternal(logger, tag, execution, logLevelFinish)
  }

  def profile[U](logger: Logger, tag: String, execution: => U, logLevelFinish: LogLevel = Info): U = {
    profileInternal(logger, tag, () => execution, logLevelFinish)
  }

  private def profileInternal[U](logger: Logger, tag: String, exec: () => U, logLevelFinish: LogLevel = Info): U = {
    logger.trace(s"Started $tag")
    val t0 = System.nanoTime()
    val result = exec()
    val t1 = System.nanoTime()
    val timeFormat = defaultTimeFormat((t1 - t0) / 1000000)
    val msg = s"$tag $timeFormat"
    logLevelFinish match {
      case Trace => logger.trace(msg)
      case Debug => logger.debug(msg)
      case Info => logger.info(msg)
      case Warn => logger.warn(msg)
      case Error => logger.error(msg)
    }
    result
  }

  private def defaultTimeFormat(timeInMillis: Long): String = {
    s"(${timeInMillis}ms)"
  }

}

object ProfilerWithErrors {
  def profileFunc[U](logger: Logger, tag: String, execution: () => U, logLevelFinish: LogLevel = Info): U = {
    val result = Profiler.profileFunc(logger, tag, execution, logLevelFinish)
    validateErrors(tag)
    result
  }

  def profile[U](logger: Logger, tag: String, execution: => U, logLevelFinish: LogLevel = Info): U = {
    val result = Profiler.profile(logger, tag, execution, logLevelFinish)
    validateErrors(tag)
    result
  }

  private def validateErrors(tag: String) = {
    Errors.validate(s => s"${s.size} errors occurred during phase: $tag")
  }
}