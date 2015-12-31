package at.vizu.s2n.error

import at.vizu.s2n.types.symbol.{Context, NoType, TScope, TType}

/**
  * Phil on 31.12.15.
  */
object TypeErrors {

  def addError(file: String, line: Int, msg: String): TType = {
    Errors.addError(file, line, msg)
    NoType
  }

  def addError(ctx: Context, msg: String): TType = {
    addError(ctx.fileName, ctx.line, msg)
  }

  def addError(scope: TScope, line: Int, msg: String): TType = {
    addError(scope.currentFile, line, msg)
  }

}
