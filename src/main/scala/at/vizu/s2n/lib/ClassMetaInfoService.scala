package at.vizu.s2n.lib

import java.nio.file.Path

import at.vizu.s2n.types.symbol.{TSymbolTable, TType}

/**
  * Phil on 02.01.16.
  */
trait ClassMetaInfoService {

  def persistClassMetaInfo(metaInfo: Seq[TType], directory: Path): Unit

  def loadClassMetaInfo(scope: TSymbolTable, directory: Path): Seq[TType]

}
