package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.types.result.ImportStmt
import at.vizu.s2n.types.symbol._

/**
  * Phil on 12.11.15.
  */
trait HeaderFileGenerator {

  protected def selfType: TType

  protected def baseTypes: BaseTypes

  protected def imports: Seq[ImportStmt]

  protected def packageName: String

  def generateHeaderFile(args: Arguments): Unit = {
    val name: String = GeneratorUtils.getHeaderFileName(selfType)
    println("Generating header file " + name)
    val content: String = GeneratorUtils.generateIncludes(imports) + generateHeaderContent(packageName)

    ScalaFiles.createDirectory(args.out)

    println("Writing header file " + name)
    ScalaFiles.writeToFile(args.out, name, content)
  }

  protected def generateHeaderContent(pkg: String): String = {
    wrapBodyWithNamespace(pkg, generateClassBody())
  }

  protected def generateClassBody(): String = {
    s"""class ${selfType.simpleName} {
        |
        |${generateSections()}
        |};
     """.stripMargin
  }

  protected def generateSections(): String = {
    groupMember().map(p => generateSection(p._1, p._2)).mkString("\n")
  }

  protected def generateSection(visibility: String, members: Seq[Modifiable]): String = {
    visibility match {
      case "public" => generatePublicSection(members)
      case _ => generateVisibilitySection(visibility, members)
    }
  }

  protected def generateVisibilitySection(visibility: String, members: Seq[Modifiable]): String = {
    val member: String = members.map(generateMember).mkString("\n")
    s"""$visibility:
       |$member
     """.stripMargin
  }

  protected def generatePublicSection(members: Seq[Modifiable]): String = {
    val publicSection: String = generateVisibilitySection("public", members)
    publicSection
  }

  protected def generateMember(member: Modifiable) = {
    member match {
      case m: Method =>
        if (m.constructor) GeneratorUtils.generateConstructorDefinition(baseTypes, m, selfType.simpleName)
        else GeneratorUtils.generateMethodDefinition(baseTypes, m)
      case f: Field => GeneratorUtils.generateFieldDefinition(baseTypes, f)
    }
  }

  protected def wrapBodyWithNamespace(pkg: String, body: String): String = {
    if (Option(pkg).exists(_.trim.nonEmpty)) {
      s"""
         |namespace ${pkg.replaceAll("\\.", "_")} {
         |
         |$body
         |}""".stripMargin
    }
    else body
  }

  protected def groupMember() = {
    val member: Seq[Modifiable] = selfType.methods ++ selfType.fields
    member.groupBy(_.visibility)
  }
}
