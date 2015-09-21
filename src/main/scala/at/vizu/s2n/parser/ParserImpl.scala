package at.vizu.s2n.parser

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

/**
 * Phil on 21.09.15.
 */
class ParserImpl extends Parser {

  override def parse(scalaContent: String): Tree = {
    val tb = scala.reflect.runtime.currentMirror.mkToolBox()
    tb.parse(scalaContent)
  }

}
