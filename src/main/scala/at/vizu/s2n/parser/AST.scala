package at.vizu.s2n.parser

import scala.reflect.runtime.universe._

/**
 * Phil on 25.09.15.
 */
case class AST(fileName: String, internalTree: Tree)