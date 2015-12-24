package at.vizu.s2n.types.symbol

import at.vizu.s2n.generator.expression.Expression

/**
  * Phil on 23.12.15.
  */
case class Parent(tpe: TType, expr: Seq[Expression] = Vector())
