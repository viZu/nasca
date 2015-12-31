package at.vizu.s2n.error

/**
  * Phil on 31.12.15.
  */
case class CError(file: String, line: Int, msg: String) {
  val formattedMessage = s"$file:$line: $msg"
}
