package at.vizu.s2n.lib

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.symbol.TSymbolTable


/**
  * Phil on 02.01.16.
  */
trait LibraryService {

  def readLibraryToScope(scope: TSymbolTable, libraryPath: Path): Unit

  def packageLibrary(scope: TSymbolTable, args: Arguments): Unit

}
