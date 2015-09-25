package at.vizu.s2n.environment

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.parser.ParserComponent

import scala.reflect.runtime.universe._

/**
 * Phil on 25.09.15.
 */
trait CppEnvironmentComponent extends EnvironmentComponent {
  this: ParserComponent =>

  class CppEnvironment extends Environment {
    override def compile(args: Arguments): Unit = {
      val contents: Seq[String] = readFileContents(args.files)
      val trees: Seq[Tree] = parser.parseContents(contents)

      val traverser: TreeTraverser = new TreeTraverser
      trees.foreach(traverser.traverse)
    }

    private def readFileContents(files: Seq[Path]) = {
      ScalaFiles.readFiles(files)
    }
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
