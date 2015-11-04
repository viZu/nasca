package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Identifier(ctx: Context, name: String, tpe: TType, mutable: Boolean) extends Nameable
