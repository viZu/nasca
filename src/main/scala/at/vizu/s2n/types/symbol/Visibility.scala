package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
object Visibility extends Enumeration {

  type Visibility = Value
  val PUBLIC = Value("public")
  val PROTECTED = Value("protected")
  val PACKAGE_PRIVATE = Value("package_private")
  val PRIVATE = Value("private")

}
