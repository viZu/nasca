package at.vizu.s2n.types.symbol

import at.vizu.s2n.types.symbol.Visibility._

/**
 * Phil on 07.10.15.
 */
case class Method(visibility: Visibility, name: String, args: Seq[Type] = Seq(), constructor: Boolean = false) {

  def checkArgs(argsToCheck: Seq[Type]) = {
    args == argsToCheck
  }

}
