package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 06.11.15.
 */
trait TypeSystemInitializer {

  def initTypeSystem(trees: Seq[AST]): TScope

}
