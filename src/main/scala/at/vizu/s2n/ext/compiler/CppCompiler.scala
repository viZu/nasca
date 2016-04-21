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
      copyHelperFiles(args)
      copyMakeFile(args)
      executeMakeFile(args)
    })
  }

  private def copyMakeFile(args: Arguments) = {
    val makeContent: String = createMakeFile(args)
    ScalaFiles.writeToFile(args.generatedDir, "Makefile", makeContent)
  }

  private def createMakeFile(args: Arguments) = {
    val makeUrl = classOf[CppCompiler].getResource("/make.template")
    val content: String = Source.fromURL(makeUrl, "UTF-8").mkString
    val binName = if (args.binType.isExecutable) args.binName else "lib" + args.binName + ".a"
    val libFlags = createLibFlags(args)
    val includeFlags = createIncludeFlags(args)
    content.replace("{{bin_name}}", binName).replace("{{i_flags}}", includeFlags).replace("{{lib_flags}}", libFlags)
  }

  private def createLibFlags(args: Arguments) = {
    args.libs.map(l => {
      val libName = "lib" + l.getFileName.toString + ".a"
      l.resolve(libName).toString
    }).mkString(" ")
  }

  private def createIncludeFlags(args: Arguments) = {
    args.libs.map(l => {
      val header = l.resolve("header").toString
      s"-I$header"
    }).mkString(" ")
  }

  private def copyAnyFiles(args: Arguments): Any = {
    copyAnyFiles("AnyRef", args)
    copyAnyFiles("Any", args)
  }

  private def copyHelperFiles(args: Arguments) = {
    copyFromResource("root_helper.h", args.generatedDir)
    copyFromResource("math_helper.h", args.generatedDir)
  }

  private def copyAnyFiles(anyString: String, args: Arguments) = {
    copyFromResource(anyString + ".cpp", args.generatedDir)
    copyFromResource(anyString + ".h", args.generatedDir)
  }

  private def copyFromResource(fileName: String, out: Path): Unit = {
    val url = classOf[CppCompiler].getResource("/" + fileName)
    val content: String = Source.fromURL(url, "UTF-8").mkString
    ScalaFiles.writeToFile(out, fileName, content)
  }

  private def executeMakeFile(args: Arguments) = {
    val outDir = args.generatedDir.toString
    val result = if (args.binType.isExecutable) executeMakeFileExecutable(outDir) else executeMakeFileLibrary(outDir)
    if (result != 0) {
      throw new ExtCompilerException(s"C++ compiler failed with exit code $result")
    }
  }

  private def executeMakeFileExecutable(outDir: String) = {
    import scala.sys.process._
    s"make -C $outDir".!(new CompilerProcessLogger)
  }

  private def executeMakeFileLibrary(outDir: String) = {
    import scala.sys.process._
    s"make library -C $outDir".!(new CompilerProcessLogger)
  }

  private class CompilerProcessLogger extends ProcessLogger with LazyLogging {
    override def out(s: => String): Unit = logger.debug(s)

    override def err(s: => String): Unit = logger.warn(s)

    override def buffer[T](f: => T): T = f
  }
}
