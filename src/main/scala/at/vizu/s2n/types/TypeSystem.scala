package at.vizu.s2n.types

import at.vizu.s2n.parser.AST

/**
 * Phil on 06.11.15.
 */
trait TypeSystem {

  def checkTrees(trees: Seq[AST]): Unit

}
