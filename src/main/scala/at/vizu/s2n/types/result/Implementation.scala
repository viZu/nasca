package at.vizu.s2n.types.result

import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 06.11.15.
 *
 * Wraps an abstract syntax tree of a trait, class or object
 */
trait Implementation {

  def generateSource(scope: TScope, packageName: String, imports: Seq[ImportStmt]): (String, String)

  def generateHeader(packageName: String, imports: Seq[ImportStmt]): (String, String)
}
