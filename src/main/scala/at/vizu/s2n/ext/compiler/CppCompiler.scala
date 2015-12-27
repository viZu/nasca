package at.vizu.s2n.ext.compiler

import java.nio.file.Path

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.log.Profiler._
import com.typesafe.scalalogging.LazyLogging

import scala.io.Source
import scala.sys.process.ProcessLogger

/**
  * Phil on 03.12.15.
  */
class CppCompiler extends ExtCompiler with LazyLogging {
  override def compile(args: Arguments): Unit = {
    profileFunc(logger, "Compiling C++ sources", () => {
      copyAnyFiles(args)
      copyMakeFile(args)
      executeMakeFile(args)
    })
  }

  private def copyMakeFile(args: Arguments) = {
    val makeContent: String = createMakeFile(args)
    ScalaFiles.writeToFile(args.out, "Makefile", makeContent)
  }

  private def createMakeFile(args: Arguments) = {
    val makeUrl = classOf[CppCompiler].getResource("/make.template")
    val content: String = Source.fromURL(makeUrl, "UTF-8").mkString
    content.replace("{{bin_name}}", args.binName)
  }

  private def copyAnyFiles(args: Arguments): Any = {
    copyAnyFiles("AnyRef", args)
    copyAnyFiles("Any", args)
  }

  private def copyAnyFiles(anyString: String, args: Arguments) = {
    copyFromResource(anyString + ".cpp", args.out)
    copyFromResource(anyString + ".h", args.out)
  }

  private def copyFromResource(fileName: String, out: Path): Unit = {
    val url = classOf[CppCompiler].getResource("/" + fileName)
    val content: String = Source.fromURL(url, "UTF-8").mkString
    ScalaFiles.writeToFile(out, fileName, content)
  }

  private def executeMakeFile(args: Arguments) = {
    import scala.sys.process._
    val outDir = args.out.toString
    val result = s"make -C $outDir".!(new CompilerProcessLogger)
    if (result < 0) {
      logger.error(s"C++ compiler exited with status code $result")
    }
  }

  private class CompilerProcessLogger extends ProcessLogger with LazyLogging {
    override def out(s: => String): Unit = logger.debug(s)

    override def err(s: => String): Unit = logger.warn(s)

    override def buffer[T](f: => T): T = f
  }
}
