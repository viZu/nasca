package at.vizu.s2n.generator

import at.vizu.s2n.conf.GlobalConfig
import at.vizu.s2n.generator.handles._
import at.vizu.s2n.types.symbol.TypeUtils._
import at.vizu.s2n.types.symbol._

/**
  * Phil on 11.11.15.
  */
object GeneratorUtils {

  def getNameSpace(packageName: String): String = {
    packageName.replaceAll("\\.", "_")
  }

  def getHeaderFileName(tpe: TType): String = {
    val name = if (tpe.isObject) tpe.simpleName + "__Object" else tpe.simpleName
    getHeaderFileName(name)
  }

  def getHeaderFileName(simpleTypeName: String): String = {
    getFileName(simpleTypeName, ".h")
  }

  def getFileName(simpleTypeName: String, fileEnding: String): String = {
    simpleTypeName + fileEnding
  }

  def getSourceFileName(tpe: TType): String = {
    val name = if (tpe.isObject) tpe.simpleName + "__Object" else tpe.simpleName
    getSourceFileName(name)
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

  def getSimpleName(baseTypes: BaseTypes, tpe: TType) = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else if (tpe.isObject) tpe.simpleName + "__Object"
    else tpe.simpleName
  }

  def generateCppTypeName(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else if (GlobalConfig.classConfig.hasRenamingHandle(tpe)) {
      GlobalConfig.classConfig.getRenamingHandle(tpe).pointerRenamer(baseTypes, tpe)
    }
    else tpe match {
      case a: AppliedGenericModifier if a.isGenericModifier => a.name
      case a: AppliedGenericModifier => generateSmartPtr(baseTypes, tpe)
      case g: GenericModifier => g.name
      case _ => generateSmartPtr(baseTypes, tpe)
    }
  }

  def getCppTypeName(baseTypes: BaseTypes, tpe: TType, withTypeDef: Boolean = false, withTypeName: Boolean = false): GeneratorContext = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else if (GlobalConfig.classConfig.hasRenamingHandle(tpe)) {
      GlobalConfig.classConfig.getRenamingHandle(tpe).typeRenamer(baseTypes, tpe)
    }
    else {
      val typeString = generateTypeArgsFromType(baseTypes, tpe, withTypeName)
      getCppTypeName(tpe.pkg, tpe.simpleName, typeString, tpe.isObject) + generateIncludeHandles(tpe)
    }
  }

  def getObjectTypeName(baseTypes: BaseTypes, tpe: TType, withTypeDef: Boolean = false, withTypeName: Boolean = false) = {
    val typeString = generateTypeArgsFromType(baseTypes, tpe, withTypeName)
    getCppTypeName(tpe.pkg, tpe.simpleName, typeString, tpe.isObject) + generateIncludeHandles(tpe)
  }

  def generateSmartPtr(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    generateSmartPtrObject(baseTypes, tpe)
  }

  def generateSmartPtrObject(baseTypes: BaseTypes, tpe: TType): GeneratorContext = {
    val ctx: GeneratorContext = getObjectTypeName(baseTypes, tpe)
    ctx.enhance(s"std::shared_ptr<$ctx>")
  }

  def getCppTypeName(pkg: String, name: String, typeString: GeneratorContext, isObject: Boolean): GeneratorContext = {
    val typeName = if (isObject) name + "__Object" else name
    if (pkg.isEmpty) typeString.enhance(typeName + typeString.content)
    else typeString.enhance(pkg.replaceAll("\\.", "_") + "::" + typeName + typeString.toString)
  }

  def generateClassBody(baseTypes: BaseTypes, tpe: TType, classContent: String): String = {
    val classTemplate: String = GeneratorUtils.generateClassTemplate(tpe)
    val extendCtx = GeneratorUtils.generateExtends(baseTypes, tpe)
    s"""${classTemplate}class ${getSimpleName(baseTypes, tpe)}$extendCtx {
       |
       |$classContent
       |};
       |""".stripMargin
  }

  def generateClassEnding(tpe: TType) = {
    val packageName = tpe.pkg
    val endif = s"${GeneratorUtils.generateEndIf(packageName, GeneratorUtils.getHeaderFileName(tpe))}"
    s"""
       |$endif""".stripMargin
  }

  def wrapBodyWithNamespace(pkg: String, body: String): String = {
    if (Option(pkg).exists(_.trim.nonEmpty)) {
      s"""
         |namespace ${GeneratorUtils.getNameSpace(pkg)} {
         |
         |$body
         |}""".stripMargin
    } else body
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

  def generateConstructorDefinition(baseTypes: BaseTypes, m: Method,
                                    withSemicolon: Boolean = true): GeneratorContext = {
    val typeName = getSimpleName(baseTypes, m.tpe)
    val paramStrings = generateParamsString(baseTypes, m.params)
    if (withSemicolon) paramStrings.enhance(s"$typeName($paramStrings);")
    else paramStrings.enhance(s"$typeName($paramStrings)")
  }

  def generateMethodDefinition(baseTypes: BaseTypes, m: Method, withSemicolon: Boolean = true): GeneratorContext = {
    val tpeName = generateCppTypeName(baseTypes, m.returnType)
    val params = generateParamsString(baseTypes, m.params, !withSemicolon)
    val mName = prettifyMethod(m.name)
    val content: String = s"${generateMethodTemplate(m)}$tpeName $mName($params)"
    if (withSemicolon) tpeName.enhance(content + ";", params.handles)
    else tpeName.enhance(content, params.handles)
  }

  def generateVirtualMethod(baseTypes: BaseTypes, m: Method): GeneratorContext = {
    val definition: GeneratorContext = generateMethodDefinition(baseTypes, m, withSemicolon = false)
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

  def generateParamsStringWithTypes(baseTypes: BaseTypes, params: Seq[TType]) = {
    val contexts: Seq[GeneratorContext] = params.map(generateCppTypeName(baseTypes, _))
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
      case agt: AppliedGenericType =>
        generateTypeArgs(baseTypes, agt.appliedTypes) + generateIncludeHandles(agt)
      case gt: GenericType =>
        GeneratorContext(generateTypeArgs(gt.getGenericModifiers, withTypeName)) + generateIncludeHandles(gt)
      case agm: AppliedGenericModifier => generateTypeArgsFromType(baseTypes, agm.getConcreteType, withTypeName)
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
    if (field.isProperty) {
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

  def generateIncludes(usedTypes: Iterable[TType]): Seq[String] = {
    Vector("#include <memory>") ++ usedTypes.map(generateInclude)
  }

  def generateInclude(tpe: TType): String = {
    if (GlobalConfig.classConfig.hasIncludeHandle(tpe)) GlobalConfig.classConfig.getIncludeHandle(tpe).content
    else {
      val headerFile = getHeaderFileName(tpe)
      s"""#include "$headerFile""""
    }
  }

  def generateCopyConstructors(baseTypes: BaseTypes, tpe: TType) = {
    val g = tpe.asInstanceOf[GenericType]
    val typeArgs = generateTypeArgs(g.getGenericModifiers)
    val tmpTemplateArgs = generateTempTemplateArgs(g.genericModifiers.size)
    val tmpTypeArgs = generateTempTypeArgs(g.genericModifiers.size)
    val typeName = getSimpleName(baseTypes, tpe)
    val fieldAssignments = generateFieldAssignments(tpe)
    val fieldInitializers = generateFieldInitializers(tpe)
    s"""
       |
       |$typeName(const $typeName& t)$fieldInitializers {}
       |$typeName& operator=(const $typeName& t){
       |  $fieldAssignments
       |}
       |
       |$tmpTemplateArgs
       |$typeName(const $typeName$tmpTypeArgs& t)$fieldInitializers {}
       |
       |$tmpTemplateArgs
       |$typeName$typeArgs& operator=(const $typeName$tmpTypeArgs& t){
       |$fieldAssignments
       |}
       |""".stripMargin
  }

  def generateCopyConstructorsHeaderTemp(tpe: TType): String = {
    generateCopyConstructorsTemp(tpe, g => {
      val typeName: String = g.simpleName
      val typeArgs = generateTypeArgs(g.getGenericModifiers)
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
         |$typeName$typeArgs& operator=(const $typeName$tmpTypeArgs& t);
         |""".stripMargin
    })
  }

  def generateCopyConstructorsSourceTemp(baseTypes: BaseTypes, tpe: TType): String = {
    generateCopyConstructorsTemp(tpe, g => {
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

  private def generateCopyConstructorsTemp(tpe: TType, f: GenericType => String): String = {
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
    if (f.isProperty) generateGetter(f)
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

  val primitiveNames = Map(RootScalaPackage + ".String" -> "std::string", RootScalaPackage + ".Boolean" -> "bool",
    RootScalaPackage + ".Unit" -> "void")

  def getPrimitiveName(primitive: TType): GeneratorContext = {
    val handles: Set[GeneratorHandle] = if (primitive.name == RootScalaPackage + ".String") {
      Set(IncludeHandle("string", AngleWrapper))
    } else {
      Set()
    }
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
