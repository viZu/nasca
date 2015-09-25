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
      opt[Seq[File]]('f', "files") valueName "<file1>,<file2>" action { (x, a) =>
        a.copy(files = x.map(_.toPath.toAbsolutePath))
      } text "files to compile - required"
      opt[String]('e', "env") valueName "<environment>" optional() action { (x, a) =>
        a.copy(env = x)
      } text "compiler environment - default 'c++'"
      opt[File]('o', "out") valueName "<directory>" action { (x, a) =>
        a.copy(out = x.toPath)
      } text s"sets the output folder - default '${Paths.get("").toAbsolutePath.toString}'"
    }
    optParser.parse(rawArgs, Arguments()) match {
      case Some(args) => args
      case _ => throw new IllegalArgumentException
    }
  }

}
