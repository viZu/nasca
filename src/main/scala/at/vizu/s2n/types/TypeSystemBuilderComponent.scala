package at.vizu.s2n.types

import at.vizu.s2n.parser.AST

/**
 * Phil on 25.09.15.
 */
trait TypeSystemBuilderComponent {

  val typeSystemBuilder: TypeSystemBuilder

  trait TypeSystemBuilder {
    def build(trees: Seq[AST]): TypeSystem
  }

}
