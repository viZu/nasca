package at.vizu.s2n.lib

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.symbol.TScope


/**
  * Phil on 02.01.16.
  */
trait LibraryService {

  def readLibraryToScope(scope: TScope, libraryPath: Path): Unit

  def packageLibrary(scope: TScope, args: Arguments): Unit

}
