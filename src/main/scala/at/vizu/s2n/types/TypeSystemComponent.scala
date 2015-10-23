package at.vizu.s2n.types

import at.vizu.s2n.parser.AST

/**
 * Phil on 08.10.15.
 */
trait TypeSystemComponent {

  val typeSystem: TypeSystem

  trait TypeSystem {

    def checkTrees(trees: Seq[AST]): Unit

  }


}
