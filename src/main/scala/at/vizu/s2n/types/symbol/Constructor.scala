package at.vizu.s2n.types.symbol

import at.vizu.s2n.types.symbol.TypeUtils._

/**
  * Phil on 05.02.16.
  */
class Constructor(ctx: Context, returnType: TType, mods: Seq[Modifier], params: Seq[Param], val primary: Boolean)
  extends Method(ctx, ConstructorName, returnType, mods, params, Vector(), true) {

  override def constructor: Boolean = true
}

object Constructor {

  def apply(ctx: Context, returnType: TType, mods: Seq[Modifier], params: Seq[Param], primary: Boolean) = {
    new Constructor(ctx, returnType, mods, params, primary)
  }

}
