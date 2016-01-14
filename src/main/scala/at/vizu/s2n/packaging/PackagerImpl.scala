package at.vizu.s2n.packaging

import java.nio.file.{Files, Path}

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.lib.LibraryService
import at.vizu.s2n.log.Profiler
import at.vizu.s2n.types.symbol.TScope
import com.typesafe.scalalogging.LazyLogging

/**
  * Phil on 05.01.16.
  */
class PackagerImpl(libraryService: LibraryService) extends Packager with LazyLogging {

  override def packageBinary(args: Arguments, scope: TScope): Unit = {
    Profiler.profileFunc(logger, "Packaging binary", () => {
      if (args.binType.isLibrary) {
        packageLibrary(args, scope)
      } else {
        packageExecutable(args)
      }
    })
  }

  private def packageLibrary(args: Arguments, scope: TScope): Unit = {
    logger.debug(s"Packaging library ${args.binName}")
    libraryService.packageLibrary(scope, args)
  }

  private def packageExecutable(args: Arguments): Unit = {
    logger.debug(s"Moving executable ${args.binName} to out dir")
    val binary: Path = args.generatedDir.resolve(args.binName)
    Files.move(binary, args.out.resolve(args.binName))
  }

}
