package at.vizu.s2n.lib

import java.nio.file.{Files, Path}

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.generator.GeneratorUtils
import at.vizu.s2n.types.symbol.{GenericType, TScope, TType}
import com.typesafe.scalalogging.LazyLogging

import scalax.file.PathMatcher

/**
  * Phil on 04.01.16.
  */
class LibraryServiceImpl(classMetaInfoService: ClassMetaInfoService) extends LibraryService with LazyLogging {

  override def readLibraryToScope(scope: TScope, libraryPath: Path): Unit = {
    logger.debug(s"Reading library ${libraryPath.toString}")
    val types: Seq[TType] = classMetaInfoService.loadClassMetaInfo(scope, libraryPath)
    scope.addAllClasses(types)
  }

  override def packageLibrary(scope: TScope, args: Arguments): Unit = {
    val libraryDirectory: Path = createLibraryDirectory(args)
    val typesToPersist = getTypesToPersist(scope)
    classMetaInfoService.persistClassMetaInfo(typesToPersist, libraryDirectory)
    moveBinary(args, libraryDirectory)
    copyNeededFiles(args.generatedDir, libraryDirectory, typesToPersist)
  }

  private def getTypesToPersist(scope: TScope) = {
    val rootScope = scope.getRootScope
    rootScope.getNonBaseTypes ++ rootScope.getNonBaseObjects
  }

  private def getBinaryName(arguments: Arguments) = {
    "lib" + arguments.binName + ".a"
  }

  private def moveBinary(args: Arguments, libDir: Path) = {
    val binName = getBinaryName(args)
    val generatedBinaryPath = args.generatedDir.resolve(binName)
    Files.move(generatedBinaryPath, libDir.resolve(binName))
  }

  private def createLibraryDirectory(args: Arguments): Path = {
    val gen: Path = args.out
    val name: String = args.binName
    val libPath: Path = gen.resolve(name) // we need the header directory
    Files.createDirectories(libPath)
    Files.createDirectories(libPath.resolve("header"))
    libPath
  }

  private def copyNeededFiles(genDir: Path, libDir: Path, typesToPersist: Seq[TType]) = {
    val neededFiles: Seq[String] = collectNeededFiles(genDir, typesToPersist)
    neededFiles.foreach(copyFile(genDir, libDir, _))
  }

  private def copyFile(genDir: Path, libDir: Path, fileName: String) = {
    val from = genDir.resolve(fileName)
    val to = libDir.resolve("header").resolve(fileName)
    if (Files.exists(from)) {
      Files.copy(from, to)
    }
  }

  private def collectNeededFiles(genDir: Path, typesToPersist: Seq[TType]): Seq[String] = {
    val path = scalax.file.Path.fromString(genDir.toString)
    val header = path.children(path.matcher(".*(\\.h)", PathMatcher.StandardSyntax.REGEX)).toSeq.map(_.name)
    val cppFiles = typesToPersist.collect({ case g: GenericType => g }).map(GeneratorUtils.getSourceFileName)
    header ++ cppFiles
  }

}
