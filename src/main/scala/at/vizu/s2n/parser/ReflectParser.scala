package at.vizu.s2n.parser

import at.vizu.s2n.log.Profiler._
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.api.JavaUniverse
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

/**
 * Phil on 06.11.15.
 */
class ReflectParser extends Parser with LazyLogging {

  override def parseContents(scalaContents: Seq[(String, String)]): Seq[AST] = {
    profileFunc(logger, "Parser", () => {
      val temp = scala.reflect.runtime.currentMirror.mkToolBox()
      val toolbox: ToolBox[JavaUniverse] = temp.asInstanceOf[ToolBox[JavaUniverse]]

      scalaContents.map(c => {
        val (filename, content) = c
        logger.debug(s"Parsing content of file $filename")
        AST(filename, parseContent(toolbox, content))
      })
    })
  }

  private def parseContent(toolbox: ToolBox[JavaUniverse], scalaContent: String): Tree = {
    toolbox.parse(scalaContent).asInstanceOf[Tree]
  }

}
