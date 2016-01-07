package at.vizu.s2n.lib

import java.nio.file.Path

import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.types.symbol.{TScope, TType}

/**
  * Phil on 02.01.16.
  */
class ClassMetaInfoServiceImpl(serializerProvider: () => ClassMetaInfoSerializer,
                               deserializerProvider: TScope => ClassMetaInfoDeserializer)
  extends ClassMetaInfoService {

  override def persistClassMetaInfo(metaInfo: Seq[TType], directory: Path): Unit = {
    val serializer = serializerProvider()
    val rawData: Array[Byte] = serializer.serialize(metaInfo)
    ScalaFiles.writeToFile(directory, MetaInfoFileName, rawData)
  }

  override def loadClassMetaInfo(scope: TScope, directory: Path): Seq[TType] = {
    val rawData = ScalaFiles.readFileRaw(directory.resolve(MetaInfoFileName))
    val deserializer = deserializerProvider(scope)
    deserializer.deserialize(rawData)
  }

}
