package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 23.10.15.
 */
trait TypeSystemInitializerComponent {

  val typeSystemInitializer: TypeSystemInitializer

  trait TypeSystemInitializer {
    def initTypeSystem(trees: Seq[AST]): TScope
  }

}
