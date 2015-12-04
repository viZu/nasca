package at.vizu.s2n.args

import java.io.File
import java.nio.file.Paths

import at.vizu.s2n.Constants
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
      opt[String]('e', "env") valueName "<environment>" optional() action { (x, a) =>
        a.copy(env = x)
      } text "compiler environment - default 'c++'"
      opt[File]('o', "out") valueName "<directory>" optional() action { (x, a) =>
        a.copy(out = x.toPath)
      } text s"sets the output folder - default '${Paths.get("").toAbsolutePath.toString}'"
      opt[String]('m', "mainClass") valueName "<mainClass>" required() action { (x, a) =>
        a.copy(main = x)
      } text "main class for execution - required"
      opt[String]('n', "binaryName") valueName "<binaryName>" optional() action { (x, a) =>
        a.copy(binName = x)
      } text "name for the binary - default 'binary'"
    }
    optParser.parse(rawArgs, Arguments()) match {
      case Some(Arguments(_, Seq(), _, _, _)) => throw new IllegalArgumentException
      case Some(args) => args
      case _ => throw new IllegalArgumentException
    }
  }

}
