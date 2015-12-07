package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle, MethodDefinitionHandle}
import at.vizu.s2n.types.result.ImportStmt
import at.vizu.s2n.types.symbol._

/**
  * Phil on 12.11.15.
  */
trait HeaderFileGenerator {

  type HandlesMap = Map[Class[_ <: GeneratorHandle], Map[String, GeneratorHandle]]

  protected def selfType: TType

  protected def baseTypes: BaseTypes

  protected def imports: Seq[ImportStmt]

  protected def packageName: String

  private var handlesMap: HandlesMap = Map()

  def generateHeaderFile(args: Arguments, handles: Seq[GeneratorHandle]): Unit = {
    this.handlesMap = handles.map(a => a.key -> a).toMap.groupBy(t => t._2.getClass)

    val name: String = GeneratorUtils.getHeaderFileName(selfType)
    println("Generating header file " + name)
    val content: String = GeneratorUtils.generateIncludes(imports) + generateHeaderContent(packageName)

    ScalaFiles.createDirectory(args.out)

    println("Writing header file " + name)
    val prettyContent = CodePrettifier.prettify(content)
    ScalaFiles.writeToFile(args.out, name, prettyContent)
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
    groupMember().map(p => generateSection(p._1, p._2)).mkString("\n\n")
  }

  protected def generateSection(visibility: String, members: Seq[Member]): String = {
    visibility match {
      case "public" => generatePublicSection(members)
      case "protected" => generateProtectedSection(members)
      case _ => generateVisibilitySection(visibility, members)
    }
  }

  protected def generateVisibilitySection(visibility: String, members: Seq[Member]): String = {
    val memberStr = if (members.isEmpty) "" else "\n" + members.map(generateMember).mkString("\n")
    s"""$visibility:$memberStr""".stripMargin
  }

  protected def generatePublicSection(members: Seq[Member]): String = {
    generateVisibilitySection("public", members)
  }

  protected def generateProtectedSection(members: Seq[Member]): String = {
    generateVisibilitySection("protected", members)
  }

  protected def generateMember(member: Member) = {
    member match {
      case m: Method =>
        if (m.constructor) GeneratorUtils.generateConstructorDefinition(baseTypes, m, selfType.simpleName)
        else if (m.isAbstract) GeneratorUtils.generateVirtualMethod(baseTypes, m)
        else GeneratorUtils.generateMethodDefinition(baseTypes, m)
      case f: Field => generateFieldDefinition(baseTypes, f)
    }
  }

  protected def generateFieldDefinition(baseTypes: BaseTypes, field: Field) = {
    val definition: String = GeneratorUtils.generateFieldDefinition(baseTypes, field)
    getHandlesMap(classOf[FieldInitializerHandle]).get(field.name)
      .map(h => definition + GeneratorUtils.generateFieldInitializer(h)).getOrElse(definition + ";")
  }

  protected def wrapBodyWithNamespace(pkg: String, body: String): String = {
    if (Option(pkg).exists(_.trim.nonEmpty)) {
      s"""
         |namespace ${GeneratorUtils.getNameSpace(pkg)} {
         |
         |$body
         |}""".stripMargin
    } else body
  }

  protected def groupMember() = {
    val methodDefinitions = getHandlesSeq(classOf[MethodDefinitionHandle]).map(_.method)
    val member: Seq[Member] = selfType.methods ++ selfType.fields ++ methodDefinitions
    member.groupBy(_.visibility)
  }

  protected def getHandlesSeq[T <: GeneratorHandle](clazz: Class[T]): Iterable[T] = {
    handlesMap.getOrElse(clazz, Map()).values.map(_.asInstanceOf[T])
  }

  protected def getHandlesMap[T <: GeneratorHandle](clazz: Class[T]): Map[String, T] = {
    handlesMap.getOrElse(clazz, Map()).asInstanceOf[Map[String, T]]
  }
}
