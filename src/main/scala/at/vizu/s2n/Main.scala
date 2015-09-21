package at.vizu.s2n

import at.vizu.s2n.args.{ArgumentsParser, Arguments}
import at.vizu.s2n.exception.ArgumentException
import at.vizu.s2n.file.Files
import at.vizu.s2n.parser.ParserImpl

import scala.reflect.runtime.universe._

/**
*  Phil on 21.09.15.
*/
object Main {

  def main(args: Array[String]) {
    try {
      val arguments = ArgumentsParser.parseArguments(args)
      val files: Seq[String] = Files.readFiles(arguments.files)

      val parser = new ParserImpl
      val trees: Seq[Tree] = parser.parseContents(files)

      val traverser: TreeTraverser = new TreeTraverser
      trees.foreach(traverser.traverse)
    } catch {
      case ae: ArgumentException => Console.err.println(ae.getMessage)
      case iae: IllegalArgumentException => System.exit(0)
    }
  }

  private def checkArgs(args: Array[String]): Unit = {
    assert(args.length > 0)
  }

  class TreeTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
      super.traverse(tree)
      println(showRaw(tree))
      println("-------------------------------")
      println()
    }
  }

}
