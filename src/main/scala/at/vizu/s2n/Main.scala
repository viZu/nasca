package at.vizu.s2n

import at.vizu.s2n.args.ArgumentsParser
import at.vizu.s2n.environment.Environments
import at.vizu.s2n.exception.{ArgumentException, TypeException}

import _root_.scala.tools.reflect.ToolBoxError

/**
*  Phil on 21.09.15.
*/
object Main {

  def main(args: Array[String]) {
    try {
      val arguments = ArgumentsParser.parseArguments(args)
      val environment = Environments(arguments)
      environment.compile(arguments)
    } catch {
      case ae: ArgumentException => Console.err.println(ae.getMessage)
      case iae: IllegalArgumentException => System.exit(0)
      case te: TypeException =>
        Console.err.println(te.formattedMessage)
        te.printStackTrace()
      case tb: ToolBoxError =>
        Console.err.println(tb.message)

    }
  }

  private def checkArgs(args: Array[String]): Unit = {
    assert(args.length > 0)
  }


}
