package at.vizu.s2n.exception

import at.vizu.s2n.error.CError
import com.typesafe.scalalogging.Logger

/**
  * Phil on 31.12.15.
  */
class CompilerException(msg: String, errors: Seq[CError]) extends Exception(msg) {

  def logErrors(logger: Logger) = {
    logger.error(msg)
    errors.foreach(e => logger.error(e.formattedMessage))
  }

}
