package at.vizu.s2n.log

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ANSIConstants._
import ch.qos.logback.core.pattern.color._

/**
  * Phil on 25.12.15.
  */
class HighlightingCompositeConverter extends ForegroundCompositeConverterBase[ILoggingEvent] {
  override def getForegroundColorCode(event: ILoggingEvent): String = {
    val level: Int = event.getLevel.toInt
    level match {
      case Level.ERROR_INT => RED_FG
      case Level.WARN_INT => YELLOW_FG
      case Level.INFO_INT => BLUE_FG
      case _ => DEFAULT_FG
    }
  }


}
