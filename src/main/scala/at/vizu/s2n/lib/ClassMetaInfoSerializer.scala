package at.vizu.s2n.lib

import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 04.01.16.
  */
trait ClassMetaInfoSerializer {

  def serialize(classInfos: Seq[TType]): Array[Byte]

}
