package at.vizu.s2n.args

import java.io.File
import java.nio.file.Paths

import at.vizu.s2n.Constants
import at.vizu.s2n.log.{Error, Info, LogLevel}
import scopt.OptionParser

/**
 * Phil on 21.09.15.
 */
object ArgumentsParser {

  def parseArguments(rawArgs: Array[String]): Arguments = {
    val optParser = new OptionParser[Arguments](Constants.CmdName) {
      head(Constants.CmdName, Constants.Version)
      opt[Seq[File]]('f', "files") required() valueName "<file1>,<file2>" action { (x, a) =>
        a.copy(files = x.map(_.toPath.toAbsolutePath))
      } text "files to compile - required"
      opt[String]('e', "environment") valueName "<environment>" optional() action { (x, a) =>
        a.copy(env = x)
      } text "compiler environment - default 'c++'"
      opt[File]('o', "out") valueName "<directory>" optional() action { (x, a) =>
        a.copy(out = x.toPath)
      } text s"sets the output folder - default '${Paths.get("").toAbsolutePath.toString}'"
      opt[String]('m', "mainclass") valueName "<mainclass>" required() action { (x, a) =>
        a.copy(main = x)
      } text "main class for execution - required"
      opt[String]('n', "binaryname") valueName "<binaryname>" optional() action { (x, a) =>
        a.copy(binName = x)
      } text "name for the binary - default 'binary'"
      opt[String]("loglevel") valueName "<loglevel>" optional() action { (x, a) =>
        a.copy(logLevel = LogLevel(x))
      } text "level for the log - default 'warn' - possibly values 'trace,debug,info,warn,error'"
      opt[String]('v', "verbose") valueName "<verbose>" optional() action { (x, a) =>
        a.copy(logLevel = Info)
      } text "verbose mode - sets the log level to 'info'"
      opt[String]('q', "quiet") valueName "<quiet>" optional() action { (x, a) =>
        a.copy(logLevel = Error)
      } text "quiet mode - sets the log level to 'error'"
      opt[String]("stdout") valueName "<stdout>" optional() action { (x, a) =>
        a.copy(stdout = Some(x))
      } text "standard out - writes the standard output to the given file"
      opt[String]("stderr") valueName "<stderr>" optional() action { (x, a) =>
        a.copy(stderr = Some(x))
      } text "standard error - writes the error output to the given file"
    }
    optParser.parse(rawArgs, Arguments()) match {
      case Some(Arguments(_, Seq(), _, _, _, _, _, _)) => throw new IllegalArgumentException
      case Some(args) => args
      case _ => throw new IllegalArgumentException
    }
  }

}
