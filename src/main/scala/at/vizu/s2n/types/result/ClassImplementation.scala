package at.vizu.s2n.types.result

import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
case class ClassImplementation(classDef: ClassDef) extends ReflectImplementation {

  override def generateString(packageName: String, imports: String): String = ???

}
