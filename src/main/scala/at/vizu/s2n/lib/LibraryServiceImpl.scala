package at.vizu.s2n.lib

import java.nio.file.{Files, Path}

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.types.symbol.{TScope, TType}

/**
  * Phil on 04.01.16.
  */
class LibraryServiceImpl(classMetaInfoService: ClassMetaInfoService) extends LibraryService {

  override def readLibraryToScope(scope: TScope, libraryPath: Path): Unit = {
    val types: Seq[TType] = classMetaInfoService.loadClassMetaInfo(scope, libraryPath)
    scope.addAllClasses(types)
  }

  override def packageLibrary(scope: TScope, args: Arguments): Unit = {
    val libraryDirectory: Path = createLibraryDirectory(args)
    val rootScope = scope.getRootScope
    val typesToPersist = rootScope.getNonBaseTypes ++ rootScope.getNonBaseObjects
    classMetaInfoService.persistClassMetaInfo(typesToPersist, libraryDirectory)
    val binName = getBinaryName(args)
    val binary = args.generatedDir.resolve(binName)
    Files.move(binary, libraryDirectory.resolve(binName))
    //TODO move header files? -> tbd
  }

  private def getBinaryName(arguments: Arguments) = {
    "lib" + arguments.binName + ".a"
  }

  private def createLibraryDirectory(args: Arguments): Path = {
    val gen: Path = args.out
    val name: String = args.binName
    val libPath: Path = gen.resolve(name)
    Files.createDirectories(libPath)
    libPath
  }
}
