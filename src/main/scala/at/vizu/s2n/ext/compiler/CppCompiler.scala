package at.vizu.s2n.ext.compiler

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles

import scala.io.Source

/**
  * Phil on 03.12.15.
  */
class CppCompiler extends ExtCompiler {
  override def compile(args: Arguments): Unit = {
    println("Compiling C++ sources")
    copyMakeFile(args)
    executeMakeFile(args)
    ???
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

  private def executeMakeFile(args: Arguments) = {
    import scala.sys.process._
    val outDir = args.out.toString
    s"make -C $outDir".!
  }
}
