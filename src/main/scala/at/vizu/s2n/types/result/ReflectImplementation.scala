package at.vizu.s2n.types.result

import at.vizu.s2n.generator.GeneratorUtils
import at.vizu.s2n.types.symbol.{Field, Method, Modifiable, TType}

import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
trait ReflectImplementation extends Implementation {

  def thisType: TType

  def generateHeader(packageName: String, imports: Seq[ImportStmt]): (String, String) = {
    (GeneratorUtils.getHeaderFileName(thisType), GeneratorUtils.generateIncludes(imports) + generateHeaderContent(packageName))
  }

  def generateBody(body: Seq[Tree]): String = {
    ""
  }

  protected def generateHeaderContent(pkg: String): String = {
    wrapBodyWithNamespace(pkg, generateClassBody())
  }

  protected def generateClassBody(): String = {
    s"""class ${thisType.simpleName} {
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
        if (m.constructor) GeneratorUtils.generateConstructorDefinition(m, thisType.simpleName)
        else GeneratorUtils.generateMethodDefinition(m)
      case f: Field => GeneratorUtils.generateFieldDefinition(f)
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
    val member: Seq[Modifiable] = thisType.methods ++ thisType.fields
    member.groupBy(_.visibility)
  }

}
