package at.vizu.s2n.types

import at.vizu.s2n.parser.AST

import scala.reflect.runtime.universe._


/**
 * Phil on 25.09.15.
 */
trait CppTypeSystemBuilderComponent extends TypeSystemBuilderComponent {

  class CppTypeSystemBuilder extends TypeSystemBuilder {
    override def build(trees: Seq[AST]): TypeSystem = {
      val traverser: TreeTraverser = new TreeTraverser
      trees.foreach { t =>
        println()
        println()
        println()
        println()
        println()
        println()
        println("************************************************************************************")
        println("************************************************************************************")
        println(s"                       ${t.fileName}")
        println("************************************************************************************")
        println("************************************************************************************")
        println()
        println()
        traverser.traverse(t.internalTree)
      }
      ???
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
