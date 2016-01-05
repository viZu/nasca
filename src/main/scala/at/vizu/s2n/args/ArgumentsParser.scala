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
      opt[Seq[File]]('l', "libs") optional() valueName "<lib1>,<lib2>" action { (x, a) =>
        a.copy(libs = x.map(_.toPath.toAbsolutePath))
      }
      opt[String]('e', "environment") valueName "<environment>" optional() action { (x, a) =>
        a.copy(env = x)
      } text "compiler environment - default 'c++'"
      opt[File]('o', "out") valueName "<directory>" optional() action { (x, a) =>
        a.copy(out = x.toPath)
      } text s"sets the output folder - default '${Paths.get("").toAbsolutePath.toString}'"
      opt[String]('m', "mainclass") valueName "<mainclass>" optional() action { (x, a) =>
        a.copy(main = x)
      } text "main class for execution - must be set if <binarytype> is set to 'exe'"
      opt[String]('n', "binaryname") valueName "<binaryname>" optional() action { (x, a) =>
        a.copy(binName = x)
      } text "name for the binary - default 'binary'"
      opt[String]("loglevel") valueName "<loglevel>" optional() action { (x, a) =>
        a.copy(logLevel = LogLevel(x))
      } validate { x =>
        if (LogLevel.stringValues.contains(x)) success
        else failure("value <loglevel> must be one of the following: trace,debug,info,warn,error")
      } text "level for the log - default 'warn' - possible values 'trace,debug,info,warn,error'"
      opt[Unit]('v', "verbose") valueName "<verbose>" optional() action { (x, a) =>
        a.copy(logLevel = Info)
      } text "verbose mode - sets the log level to 'info'"
      opt[Unit]('q', "quiet") valueName "<quiet>" optional() action { (x, a) =>
        a.copy(logLevel = Error)
      } text "quiet mode - sets the log level to 'error'"
      opt[String]("stdout") valueName "<stdout>" optional() action { (x, a) =>
        a.copy(stdout = Some(x))
      } text "standard out - writes the standard output to the given file"
      opt[String]("stderr") valueName "<stderr>" optional() action { (x, a) =>
        a.copy(stderr = Some(x))
      } text "standard error - writes the error output to the given file"
      opt[String]('t', "binarytype") valueName "<binarytype>" optional() action { (x, a) =>
        a.copy(binType = BinaryType(x))
      } validate { x =>
        if (BinaryType.stringValues.contains(x)) success else failure("value <binarytype> must be either 'exe' or 'lib'")
      } text "binary type - sets the binary type - possible values 'exe,lib' - default 'exe'"
      checkConfig { c =>
        if (c.binType == Executable && c.main == "") failure("value <mainclass> must be set if <binarytype>='exe'")
        else if (c.files.isEmpty) failure("<files> must contain at least 1 file")
        else success
      }
    }
    optParser.parse(rawArgs, Arguments()) match {
      case Some(args) => args
      case _ => throw new IllegalArgumentException
    }
  }

}
