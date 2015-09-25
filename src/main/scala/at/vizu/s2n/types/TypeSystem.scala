package at.vizu.s2n.types

import scala.reflect.runtime.universe._

/**
 * Phil on 25.09.15.
 */
trait TypeSystem {

  def checkTrees(trees: Seq[Tree]) = trees.foreach(checkTree)

  def checkTree(tree: Tree): Unit

}
