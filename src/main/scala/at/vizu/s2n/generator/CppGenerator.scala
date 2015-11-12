package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.types.result.ScalaFileWrapper

/**
 * Phil on 06.11.15.
 */
class CppGenerator extends Generator {

  type GeneratedFile = (String, String)

  override def generateCode(args: Arguments, fileContents: Seq[ScalaFileWrapper]): Unit = {
    println("Generating header files...")
    val headerFiles: Seq[GeneratedFile] = fileContents.flatMap(c => {
      c.impls.map(i => {
        val h = i.generateHeader(c.pkg, c.imports)
        println(h._1)
        h
      })
    })

    println("Writing C++ files into output directory " + args.out.toString)
    ScalaFiles.createDirectory(args.out)

    println("Writing header files...")
    headerFiles.foreach(h => {
      println(h._1)
      ScalaFiles.writeToFile(args.out, h._1, h._2)
    })
  }

}
