package at.vizu.s2n.types.result

import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
case class ClassImplementation(classDef: ClassDef, tpe: TType) extends ReflectImplementation
