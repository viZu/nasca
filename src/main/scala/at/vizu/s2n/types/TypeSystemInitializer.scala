package at.vizu.s2n.types

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.TSymbolTable

/**
 * Phil on 06.11.15.
 */
trait TypeSystemInitializer {

  def initTypeSystem(args: Arguments, trees: Seq[AST]): TSymbolTable

}
