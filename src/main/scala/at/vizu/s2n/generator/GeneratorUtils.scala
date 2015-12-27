package at.vizu.s2n.generator

import at.vizu.s2n.generator.handles._
import at.vizu.s2n.types.symbol._

/**
  * Phil on 11.11.15.
  */
object GeneratorUtils {

  def getNameSpace(packageName: String): String = {
    packageName.replaceAll("\\.", "_")
  }

  def getHeaderFileName(tpe: TType): String = {
    getHeaderFileName(tpe.simpleName)
  }

  def getHeaderFileName(simpleTypeName: String): String = {
    getFileName(simpleTypeName, ".h")
  }

  def getFileName(simpleTypeName: String, fileEnding: String): String = {
    simpleTypeName + fileEnding
  }

  def getSourceFileName(tpe: TType): String = {
    getSourceFileName(tpe.simpleName)
  }

  def getSourceFileName(simpleTypeName: String): String = {
    getFileName(simpleTypeName, ".cpp")
  }

  def generateIncludeHandles(tpe: TType): Option[GeneratorHandle] = {
    def generateIncludeHandle() = {
      IncludeHandle(getHeaderFileName(tpe), QuotationWrapper)
    }
    tpe match {
      case a: AppliedGenericModifier => a.getConcreteType match {
        case g: GenericModifier => None
        case _@t => Some(generateIncludeHandle())
      }
      case g: GenericModifier => None
      case _@t => Some(generateIncludeHandle())
    }
  }

  def generateIncludeGuard(pkg: String, fileName: String): String = {
    val guardIdentifier = generateGuardIdentifier(pkg, fileName)
    s"""#ifndef $guardIdentifier
        |#define $guardIdentifier
        |
     """.stripMargin
  }

  def generateEndIf(pkg: String, fileName: String): String = {
    val guardIdentifier = generateGuardIdentifier(pkg, fileName)
    s"""#endif //$guardIdentifier"""
  }

  def generateGuardIdentifier(pkg: String, fileName: String) = {
    val guardIdentifier: String = (getNameSpace(pkg) + "_" + fileName.replaceAll("\\.", "_")).toUpperCase
    guardIdentifier.toUpperCase
  }

  def generateCppTypeName(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else tpe match {
      case a: AppliedGenericModifier if a.isGenericModifier => a.name
      case a: AppliedGenericModifier => generateSmartPtr(baseTypes, tpe)
      case g: GenericModifier => g.name
      case _ => generateSmartPtr(baseTypes, tpe)
    }
  }

  def getCppTypeName(baseTypes: BaseTypes, tpe: TType, withTypeDef: Boolean = false, withTypeName: Boolean = false): GeneratorContext = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else {
      val typeString = generateTypeArgsFromType(baseTypes, tpe, withTypeName)
      getCppTypeName(tpe.pkg, tpe.simpleName, typeString) + generateIncludeHandles(tpe)
    }
  }

  def getObjectTypeName(baseTypes: BaseTypes, tpe: TType, withTypeDef: Boolean = false, withTypeName: Boolean = false) = {
    val typeString = generateTypeArgsFromType(baseTypes, tpe, withTypeName)
    getCppTypeName(tpe.pkg, tpe.simpleName, typeString) + generateIncludeHandles(tpe)
  }

  def generateSmartPtr(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    generateSmartPtrObject(baseTypes, tpe)
  }

  def generateSmartPtrObject(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    val ctx: GeneratorContext = getObjectTypeName(baseTypes, tpe)
    ctx.enhance(s"std::shared_ptr<$ctx>")
  }

  def getCppTypeName(pkg: String, name: String, typeString: GeneratorContext): GeneratorContext = {
    if (pkg.isEmpty) typeString.enhance(name + typeString.content)
    else typeString.enhance(pkg.replaceAll("\\.", "_") + "::" + name + typeString.toString)
  }

  def generateClassTemplate(tpe: TType): String = {
    tpe match {
      case gt: GenericType =>
        val templateString = GeneratorUtils.generateTemplatesString(gt.genericModifiers, withTypeName = true)
        s"""$templateString
           |""".stripMargin
      case _ => ""
    }
  }

  def generateExtends(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    tpe.parentTypes match {
      case Seq() => ""
      case parents => mergeGeneratorContexts(parents.map(generateSingleExtend(baseTypes, _)), ", ", startsWith = " : ")
    }
  }

  def generateSingleExtend(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    val extend: GeneratorContext = getCppTypeName(baseTypes, tpe)
    extend.enhance(s"public $extend")
  }

  def generateConstructorDefinition(baseTypes: BaseTypes, m: Method, typeName: String): GeneratorContext = {
    val paramStrings = generateParamsString(baseTypes, m.params)
    paramStrings.enhance(s"$typeName($paramStrings);")
  }

  def generateMethodDefinition(baseTypes: BaseTypes, m: Method): GeneratorContext = {
    val tpeName = generateCppTypeName(baseTypes, m.returnType)
    val params = generateParamsString(baseTypes, m.params)
    val mName = prettifyMethod(m.name)
    tpeName.enhance(s"${generateMethodTemplate(m)}$tpeName $mName($params);", params.handles)
  }

  def generateVirtualMethod(baseTypes: BaseTypes, m: Method): GeneratorContext = {
    val ctx: GeneratorContext = generateMethodDefinition(baseTypes, m)
    val definition: GeneratorContext = ctx.enhance(ctx.content.trim.dropRight(1)) // remove semicolon
    definition.enhance(s"${generateMethodTemplate(m)}virtual $definition = 0;")
  }

  def generateMethodTemplate(m: Method): String = {
    if (m.generics.isEmpty) ""
    else
      s"""${generateTemplatesString(m.generics, withTypeName = true)}
         |""".stripMargin
  }

  def generateParamsString(baseTypes: BaseTypes, params: Seq[Param], withVars: Boolean = false) = {
    val contexts: Seq[GeneratorContext] = params.map(p => {
      val ctx: GeneratorContext = generateCppTypeName(baseTypes, p.tpe)
      if (withVars) ctx + s" ${p.name}" else ctx
    })
    mergeGeneratorContexts(contexts, ", ")
  }

  def generateTemplatesStringFromType(tpe: TType, withTypeName: Boolean = false): String = {
    tpe match {
      case gt: GenericType => generateTemplatesString(gt.genericModifiers, withTypeName)
      case _ => ""
    }
  }

  def generateTemplatesString(generics: Seq[GenericModifier], withTypeName: Boolean = false): String = {
    val typeArgs = generateTypeArgs(generics, withTypeName)
    s"template$typeArgs"
  }

  def generateTemplatesStringFromString(generics: Seq[String], withTypeName: Boolean = false): String = {
    val typeArgs = generateTypeArgsString(generics, withTypeName)
    s"template$typeArgs"
  }

  def generateTypeArgsFromType(baseTypes: BaseTypes, tpe: TType, withTypeName: Boolean = false): GeneratorContext = {
    tpe match {
      case agt: AppliedGenericType => generateTypeArgs(baseTypes, agt.appliedTypes) + generateIncludeHandles(agt)
      case gt: GenericType =>
        GeneratorContext(generateTypeArgs(gt.genericModifiers, withTypeName)) + generateIncludeHandles(gt)
      case _ => generateIncludeHandles(tpe)
    }
  }

  def generateTypeArgs(baseTypes: BaseTypes, typeArgs: Seq[TType]): GeneratorContext = {
    val ctxs = typeArgs.map(generateCppTypeName(baseTypes, _))
    val context = mergeGeneratorContexts(ctxs, ", ")
    context.enhance(s"<$context>")
  }

  def generateTypeArgs(generics: Seq[GenericModifier], withTypeName: Boolean = false): String = {
    val templateTypes = generics.map(if (withTypeName) "typename " + _.name else _.name).mkString(", ")
    s"<$templateTypes>"
  }

  def generateTypeArgsString(generics: Seq[String], withTypeName: Boolean = false): String = {
    val templateTypes = generics.map(s => if (withTypeName) "typename " + s else s).mkString(", ")
    s"<$templateTypes>"
  }

  def generateFieldDefinition(baseTypes: BaseTypes, f: Field): String = {
    val const = if (f.isMutable) "" else "const "
    s"  $const${generateCppTypeName(baseTypes, f.tpe)} ${f.name}"
  }

  def generateFieldInitializer(handle: FieldInitializerHandle): String = {
    s" = ${handle.content};"
  }

  def generateParamAccessor(baseTypes: BaseTypes, field: Field): String = {
    if (field.isPublicField) {
      Vector(generateGetterDefinition(baseTypes, field), generateSetterDefinition(baseTypes, field))
        .filter(_.nonEmpty)
        .mkString("\n")
    } else {
      generateGetterDefinition(baseTypes, field)
    }
  }

  def generateGetterDefinition(baseTypes: BaseTypes, field: Field): String = {
    val retVal = GeneratorUtils.generateCppTypeName(baseTypes, field.tpe)
    s"$retVal ${GeneratorUtils.generateGetter(field)} { return ${field.name}; }"
  }

  def generateSetterDefinition(baseTypes: BaseTypes, field: Field): String = {
    if (field.isMutable) {
      val nameUpper = field.name.toUpperCase
      val setTpe = GeneratorUtils.generateCppTypeName(baseTypes, field.tpe)
      s"void set$nameUpper($setTpe newVal) { ${field.name} = newVal; }"
    } else {
      ""
    }
  }

  def generateGetter(field: Field): String = {
    generateGetter(field.name)
  }

  def generateGetter(fieldName: String): String = {
    val nameUpper = fieldName.toUpperCase
    s"get__$nameUpper()"
  }

  def generateIncludes(usedTypes: Iterable[TType]): String = {
    (Vector("#include <memory>") ++ usedTypes.map(t => {
      val headerFile = getHeaderFileName(t)
      s"""#include "$headerFile""""
    })).mkString("\n")
  }

  def generateCopyConstructorsHeader(tpe: TType): String = {
    generateCopyConstructors(tpe, g => {
      val typeName: String = tpe.simpleName
      val tmpTemplateArgs = generateTempTemplateArgs(g.genericModifiers.size)
      val tmpTypeArgs = generateTempTypeArgs(g.genericModifiers.size)
      s"""
         |
           |$typeName(const $typeName& t);
         |$typeName& operator=(const $typeName& t);
         |
           |$tmpTemplateArgs
         |$typeName(const $typeName$tmpTypeArgs& t);
         |
           |$tmpTemplateArgs
         |$typeName<T>& operator=(const $typeName$tmpTypeArgs& t);
         |""".stripMargin
    })
  }

  def generateCopyConstructorsSource(baseTypes: BaseTypes, tpe: TType): String = {
    generateCopyConstructors(tpe, g => {
      val typeName: String = tpe.simpleName
      val typeArgs = generateTemplatesString(g.genericModifiers, withTypeName = true)
      val tmpTemplateArgs = generateTempTemplateArgs(g.genericModifiers.size)
      val tmpTypeArgs = generateTempTypeArgs(g.genericModifiers.size)
      val cppTypename = getCppTypeName(baseTypes, tpe, withTypeName = false).content
      val fieldAssignments = generateFieldAssignments(tpe)
      val fieldInitializers = generateFieldInitializers(tpe)
      s"""
         |
           |$typeArgs
         |$cppTypename::$typeName(const $typeName& t)$fieldInitializers {}
         |
           |$typeArgs
         |$cppTypename &$cppTypename::operator=(const $typeName& t) {
         |$fieldAssignments
         |}
         |
           |$typeArgs
         |$tmpTemplateArgs
         |$cppTypename::$typeName(const $typeName$tmpTypeArgs& t)$fieldInitializers {}
         |
           |$typeArgs
         |$tmpTemplateArgs
         |$cppTypename &$cppTypename::operator=(const $typeName$tmpTypeArgs& t) {
         |$fieldAssignments
         |}
         |""".stripMargin
    })
  }

  private def generateCopyConstructors(tpe: TType, f: GenericType => String): String = {
    tpe match {
      case a: AppliedGenericType => ""
      case g: GenericType => f(g)
      case _ => ""
    }
  }

  private def generateTempTemplateArgs(genericModifierSize: Int) = {
    val temp = "TEMP_TYPE"
    val genStrings = (0 until genericModifierSize).map(n => temp + n)
    generateTemplatesStringFromString(genStrings, withTypeName = true)
  }

  private def generateTempTypeArgs(genericModifierSize: Int) = {
    val temp = "TEMP_TYPE"
    val genStrings = (0 until genericModifierSize).map(n => temp + n)
    generateTypeArgsString(genStrings, withTypeName = false)
  }

  private def generateFieldAssignments(tpe: TType) = {
    tpe.fields.map(generateFieldAssignment).mkString("\n")
  }

  private def generateFieldAssignment(f: Field) = {
    val fieldName = f.name
    val getter = generateGetter(fieldName)
    s"this->$fieldName = t.$getter;"
  }

  def generateFieldAccessor(f: Field) = {
    if (f.isPublicField) generateGetter(f)
    else f.name
  }

  private def generateFieldInitializers(tpe: TType) = {
    tpe.fields.map(generateFieldInitializer).mkString(",") match {
      case "" => ""
      case s => " : " + s
    }
  }

  private def generateFieldInitializer(f: Field) = {
    val fieldName = f.name
    val getter = generateGetter(fieldName)
    s"$fieldName(t.$getter)"
  }

  val primitiveNames = Map("scala.String" -> "std::string", "scala.Boolean" -> "bool", "scala.Unit" -> "void")

  def getPrimitiveName(primitive: TType): GeneratorContext = {
    val handles: Set[GeneratorHandle] = if (primitive.name == "scala.String") Set(IncludeHandle("string", AngleWrapper)) else Set()
    val primitiveName: String = primitiveNames.getOrElse(primitive.name, primitive.simpleName.toLowerCase)
    GeneratorContext(primitiveName, handles)
  }

  def prettifyMethod(m: String) = {
    m.replaceAll("\\$", "__")
  }

  def mergeGeneratorContexts(seq: Seq[GeneratorContext], seperator: String = "\n",
                             endsWith: String = "", startsWith: String = "", givenContent: String = null): GeneratorContext = {
    val content: String = if (givenContent != null) {
      givenContent
    } else {
      startsWith + seq.filter(_.definedContent).map(expr => expr.content).mkString(seperator) + endsWith // remove empty contents
    }
    val handles: Set[GeneratorHandle] = seq.flatMap(_.handles).toSet
    GeneratorContext(content, handles)
  }
}
