package at.vizu.s2n.types.result

import at.vizu.s2n.types.symbol.TType

import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
case class ObjectImplementation(module: ModuleDef, tpe: TType) extends ReflectImplementation