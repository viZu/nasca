package at.vizu.s2n.lib

import at.vizu.s2n.types.symbol.TSymbolTable

/**
  * Phil on 04.01.16.
  */
trait CppLibraryComponent {

  import com.softwaremill.macwire._

  lazy val classMetainfoSerializerProvider = () => wire[ProtoBufClassMetaInfoSerializer]
  lazy val classMetainfoDeserializerProvider = (scope: TSymbolTable) => wire[ProtoBufClassMetaInfoDeserializer]

  lazy val classMetaInfoService = wire[ClassMetaInfoServiceImpl]
  lazy val libraryService = wire[LibraryServiceImpl]

}
