package at.vizu.s2n.types

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.result.ScalaFileWrapper
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 06.11.15.
 */
trait TypeSystem {

  def checkTrees(args: Arguments, trees: Seq[AST]): (TScope, Seq[ScalaFileWrapper])

}
