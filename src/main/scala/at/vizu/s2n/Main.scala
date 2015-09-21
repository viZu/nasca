package at.vizu.s2n

import at.vizu.s2n.file.Files
import at.vizu.s2n.parser.{ParserImpl}

/**
*  Phil on 21.09.15.
*/
object Main {

  def main(args: Array[String]) {
    val file: String = Files.readFile(args(0))
    var parser = new ParserImpl
  }

}
