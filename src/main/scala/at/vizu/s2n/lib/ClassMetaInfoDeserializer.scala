package at.vizu.s2n.lib

import at.vizu.s2n.types.symbol.TType

/**
  * Phil on 04.01.16.
  */
trait ClassMetaInfoDeserializer {

  def deserialize(bytes: Array[Byte]): Seq[TType]

}
