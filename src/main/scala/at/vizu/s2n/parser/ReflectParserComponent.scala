package at.vizu.s2n.parser

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

/**
 * Phil on 25.09.15.
 */
trait ReflectParserComponent extends ParserComponent {

  class ReflectParser extends Parser {
    override def parseContents(scalaContents: Seq[String]): Seq[Tree] = scalaContents.map(parseContent)

    override def parseContent(scalaContent: String): Tree = {
      val tb = scala.reflect.runtime.currentMirror.mkToolBox()
      tb.parse(scalaContent)
    }
  }

}
