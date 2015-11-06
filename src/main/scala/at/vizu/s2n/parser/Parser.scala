package at.vizu.s2n.parser

/**
 * Phil on 06.11.15.
 */
trait Parser {

  def parseContents(scalaContents: Seq[(String, String)]): Seq[AST]

}
