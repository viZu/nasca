package at.vizu.s2n.log

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.AbstractMatcherFilter
import ch.qos.logback.core.spi.FilterReply

/**
  * Phil on 28.12.15.
  */
class MaxLevelFilter extends AbstractMatcherFilter[ILoggingEvent] {

  var maxLevel: Level = null

  override def decide(event: ILoggingEvent): FilterReply = {
    if (!isStarted) FilterReply.NEUTRAL
    else if (event.getLevel.levelInt > maxLevel.levelInt) {
      FilterReply.DENY
    } else {
      FilterReply.NEUTRAL
    }
  }

  def setMaxLevel(levelStr: String) = maxLevel = Level.toLevel(levelStr)

  def setMaxLevel(level: Level) = maxLevel = level

  override def start(): Unit = if (maxLevel != null) super.start()
}
