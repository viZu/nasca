package at.vizu.s2n

import at.vizu.s2n.args.{Arguments, ArgumentsParser}
import at.vizu.s2n.environment.Environments
import at.vizu.s2n.exception.{ArgumentException, TypeException}
import at.vizu.s2n.log.{Log, LogOptions}
import com.typesafe.scalalogging.LazyLogging

import _root_.scala.tools.reflect.ToolBoxError

/**
*  Phil on 21.09.15.
*/
object Main extends LazyLogging {


  def main(args: Array[String]) {
    try {
      val arguments = ArgumentsParser.parseArguments(args)
      initLogger(arguments)
      val environment = Environments(arguments)
      environment.compile(arguments)
    } catch {
      case ae: ArgumentException => logger.error("Wrong arguments", ae)
      case iae: IllegalArgumentException => logger.error("Illegal arguments", iae)
      case te: TypeException => logger.error(te.formattedMessage, te)
      case e: Exception => logger.error("An error occurred")
      case tb: ToolBoxError => logger.error("Error initializing scala toolbox", tb)
    }
  }

  private def checkArgs(args: Array[String]): Unit = {
    assert(args.length > 0)
  }

  private def initLogger(args: Arguments) = {
    val options: LogOptions = LogOptions(args)
    Log.init(options)
  }

}
