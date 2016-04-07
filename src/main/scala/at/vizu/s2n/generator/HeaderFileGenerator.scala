package at.vizu.s2n.generator

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.file.ScalaFiles
import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle, MethodDefinitionHandle}
import at.vizu.s2n.types.symbol._
import com.typesafe.scalalogging.LazyLogging

/**
  * Phil on 12.11.15.
  */
trait HeaderFileGenerator extends LazyLogging {

  type HandlesMap = Map[Class[_ <: GeneratorHandle], Map[String, GeneratorHandle]]

  protected def selfType: TType

  protected def baseTypes: BaseTypes

  protected def packageName: String

  private var handlesMap: HandlesMap = Map()

  def generateHeaderFile(args: Arguments, handles: Set[GeneratorHandle]): Unit = {
    this.handlesMap = handles.map(a => a.key -> a).toMap.groupBy(t => t._2.getClass)

    val name: String = GeneratorUtils.getHeaderFileName(selfType)
    logger.debug("Generating header file " + name)
    val includeGuard = GeneratorUtils.generateIncludeGuard(packageName, GeneratorUtils.getHeaderFileName(selfType))

    val content: String = includeGuard + generateIncludes() + generateHeaderContent(packageName)

    logger.debug("Writing header file " + name)
    val prettyContent = CodePrettifier.prettify(content)
    ScalaFiles.writeToFile(args.generatedDir, name, prettyContent)
  }

  private def generateIncludes() = {
    val usedTypes = TypeUtils.getUsedTypes(baseTypes, selfType)
    (GeneratorUtils.generateIncludes(usedTypes) :+ "").mkString("\n")
  }

  protected def generateHeaderContent(pkg: String): String = {
    wrapBodyWithNamespace(pkg, generateClassBody()) + GeneratorUtils.generateClassEnding(selfType)
  }

  protected def generateClassBody(): String = {
    GeneratorUtils.generateClassBody(baseTypes, selfType, generateSections())
  }

  protected def generateSections(): String = {
    groupMember().map(p => generateSection(p._1, p._2)).mkString("\n\n")
  }

  protected def generateSection(visibility: String, members: Seq[String]) = visibility match {
      case "public" => generatePublicSection(members)
      case "protected" => generateProtectedSection(members)
      case _ => generateVisibilitySection(visibility, members)
  }

  protected def generateVisibilitySection(visibility: String, members: Seq[String]): String = {
    val memberStr = if (members.isEmpty) "" else "\n" + members.mkString("\n")
    s"""$visibility:$memberStr""".stripMargin
  }

  protected def generatePublicSection(members: Seq[String]): String = {
    generateVisibilitySection("public", members)
  }

  protected def generateProtectedSection(members: Seq[String]): String = {
    generateVisibilitySection("protected", members)
  }

  protected def generateMember(member: Member) = {
    member match {
      case m: Method => generateMethodDefinition(m).value
      case f: Field => generateFieldDefinition(baseTypes, f)
    }
  }

  protected def generateMethodDefinition(m: Method): GeneratorContext = {
    if (m.constructor) GeneratorUtils.generateConstructorDefinition(baseTypes, m)
    else if (m.isAbstract) GeneratorUtils.generateVirtualMethod(baseTypes, m)
    else GeneratorUtils.generateMethodDefinition(baseTypes, m)
  }

  protected def generateFieldDefinition(baseTypes: BaseTypes, field: Field): String = {
    val definition: String = GeneratorUtils.generateFieldDefinition(baseTypes, field)
    val d = getHandlesMap(classOf[FieldInitializerHandle]).get(field.name)
      .map(h => definition + GeneratorUtils.generateFieldInitializer(h)).getOrElse(definition + ";")
    val accessors: String = GeneratorUtils.generateParamAccessor(baseTypes, field)
    d + (if (accessors.nonEmpty) "\n" + accessors + "\n" else "")
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
    val methods = selfType.methods ++ methodDefinitions
    val tuples = generateMethods(methods) ++ generateFields(selfType.fields)
    tuples.groupBy(_._1).mapValues(sq => sq.map(_._2))
  }

  protected def generateMethods(methods: Seq[Method]): Seq[(String, String)] = {
    methods.map(m => (m.visibility, generateMember(m)))
  }

  protected def generateFields(fields: Seq[Field]): Seq[(String, String)] = {
    fields.flatMap(generateField)
  }

  private def generateField(field: Field): Seq[(String, String)] = {
    val definition: String = GeneratorUtils.generateFieldDefinition(baseTypes, field)
    val d = getHandlesMap(classOf[FieldInitializerHandle]).get(field.name)
      .map(h => definition + GeneratorUtils.generateFieldInitializer(h)).getOrElse(definition + ";")
    val accessors = ("public", GeneratorUtils.generateParamAccessor(baseTypes, field))
    val fieldDefinition = ("private", d)
    Vector(accessors, fieldDefinition)
  }

  protected def getHandlesSeq[T <: GeneratorHandle](clazz: Class[T]): Iterable[T] = {
    handlesMap.getOrElse(clazz, Map()).values.map(_.asInstanceOf[T])
  }

  protected def getHandlesMap[T <: GeneratorHandle](clazz: Class[T]): Map[String, T] = {
    handlesMap.getOrElse(clazz, Map()).asInstanceOf[Map[String, T]]
  }
}
