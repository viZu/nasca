package at.vizu.s2n.scala.reflect.api

import scala.reflect.runtime.universe._

/**
  * Phil on 22.11.15.
  */

case class InlineBlock(block: Block) {
  def stats: List[Tree] = block.stats

  def expr: Tree = block.expr
}
