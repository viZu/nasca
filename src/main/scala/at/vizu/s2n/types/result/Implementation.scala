package at.vizu.s2n.types.result

/**
 * Phil on 06.11.15.
 *
 * Wraps an abstract syntax tree of a trait, class or object
 */
trait Implementation {

  def generateString(packageName: String, imports: String): String

}
