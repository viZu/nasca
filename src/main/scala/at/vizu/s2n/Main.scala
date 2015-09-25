package at.vizu.s2n

import at.vizu.s2n.args.ArgumentsParser
import at.vizu.s2n.environment.Environment
import at.vizu.s2n.exception.ArgumentException

/**
*  Phil on 21.09.15.
*/
object Main {

  def main(args: Array[String]) {
    try {
      val arguments = ArgumentsParser.parseArguments(args)
      val environment = Environment(arguments)
      environment.compile(arguments)
    } catch {
      case ae: ArgumentException => Console.err.println(ae.getMessage)
      case iae: IllegalArgumentException => System.exit(0)
    }
  }

  private def checkArgs(args: Array[String]): Unit = {
    assert(args.length > 0)
  }


}
