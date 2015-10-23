package at.vizu.s2n.exception

/**
 * Phil on 16.10.15.
 */
class TypeException(file: String, line: Int, message: String) extends RuntimeException(message) {

  val formattedMessage = s"$file:$line: error: $message"

}
