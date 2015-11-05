package at.vizu.s2n.exception

import at.vizu.s2n.types.symbol.Context

/**
 * Phil on 16.10.15.
 */
class TypeException(file: String, line: Int, message: String) extends RuntimeException(message) {
  def this(ctx: Context, message: String) = this(ctx.fileName, ctx.line, message)
  val formattedMessage = s"$file:$line: error: $message"

}
