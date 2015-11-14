package at.vizu.s2n.types.result

import at.vizu.s2n.types.symbol.TType

/**
 * Phil on 06.11.15.
 *
 * Wraps an abstract syntax tree of a trait, class or object
 */
trait Implementation {

  def tpe: TType

}
