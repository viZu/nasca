package at.vizu.s2n.parser

import scala.reflect.runtime.universe._

/**
 * Phil on 21.09.15.
 */
trait Parser {

  def parseContents(scalaContents: Seq[String]): Seq[Tree]

  def parseContent(scalaContent: String): Tree

}
