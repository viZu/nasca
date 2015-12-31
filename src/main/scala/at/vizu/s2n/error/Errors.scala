package at.vizu.s2n.error

import at.vizu.s2n.exception.CompilerException
import at.vizu.s2n.types.symbol.{Context, TScope}

/**
  * Phil on 31.12.15.
  */
object Errors {

  var errors: Seq[CError] = Vector()

  def addError(file: String, line: Int, msg: String): Unit = {
    errors = errors :+ CError(file, line, msg)
  }

  def addError(ctx: Context, msg: String): Unit = {
    addError(ctx.fileName, ctx.line, msg)
  }

  def addError(scope: TScope, line: Int, msg: String): Unit = {
    addError(scope.currentFile, line, msg)
  }

  def validate(f: Seq[CError] => String) = {
    if (errors.nonEmpty) {
      throw new CompilerException(f(errors), errors)
    }
  }
}
