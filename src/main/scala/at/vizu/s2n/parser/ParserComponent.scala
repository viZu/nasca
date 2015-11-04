package at.vizu.s2n.parser

/**
 * Phil on 25.09.15.
 */
trait ParserComponent {

  val parser: Parser

  trait Parser {
    def parseContents(scalaContents: Seq[(String, String)]): Seq[AST]
  }

}
