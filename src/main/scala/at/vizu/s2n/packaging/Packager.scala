package at.vizu.s2n.packaging

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.symbol.TSymbolTable

/**
  * Phil on 05.01.16.
  */
trait Packager {

  def packageBinary(args: Arguments, scope: TSymbolTable)

}
