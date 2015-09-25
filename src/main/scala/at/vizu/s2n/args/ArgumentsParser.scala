package at.vizu.s2n.args

import java.io.File

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
        a.copy(files = x.map(_.toPath))
      } text "Files to compile"
      opt[String]('e', "environment") valueName "environment" optional() action { (x, a) =>
        a.copy(env = x)
      }
    }
    optParser.parse(rawArgs, Arguments()) match {
      case Some(args) => args
      case _ => throw new IllegalArgumentException
    }
  }

}
