package at.vizu.s2n.generator

import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle, IncludeHandle}
import at.vizu.s2n.types.result.ImportStmt
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
      case a: AppliedGenericModifier => generateSmartPtr(baseTypes, tpe)
      case g: GenericModifier => g.name
      case _ => generateSmartPtr(baseTypes, tpe)
    }
  }

  def getCppTypeName(baseTypes: BaseTypes, tpe: TType, withTypeDef: Boolean = false, withTypeName: Boolean = false): GeneratorContext = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else {
      val typeString = generateTypeArgsFromType(baseTypes, tpe, withTypeName)
      getCppTypeName(tpe.pkg, tpe.simpleName, typeString)
    }
  }

  def getObjectTypeName(baseTypes: BaseTypes, tpe: TType, withTypeDef: Boolean = false, withTypeName: Boolean = false) = {
    val typeString = generateTypeArgsFromType(baseTypes, tpe, withTypeName)
    getCppTypeName(tpe.pkg, tpe.simpleName, typeString)
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

  def generateConstructorDefinition(baseTypes: BaseTypes, m: Method, typeName: String): GeneratorContext = {
    val paramStrings = generateParamsString(baseTypes, m.params)
    paramStrings.enhance(s"$typeName($paramStrings);")
  }

  def generateMethodDefinition(baseTypes: BaseTypes, m: Method): GeneratorContext = {
    val tpeName = generateCppTypeName(baseTypes, m.returnType)
    val params = generateParamsString(baseTypes, m.params)
    tpeName.enhance(s"${generateMethodTemplate(m)}$tpeName ${m.name}($params);", params.handles)
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

  def generateTypeArgsFromType(baseTypes: BaseTypes, tpe: TType, withTypeName: Boolean = false): GeneratorContext = {
    tpe match {
      case agt: AppliedGenericType => generateTypeArgs(baseTypes, agt.appliedTypes)
      case gt: GenericType => generateTypeArgs(gt.genericModifiers, withTypeName)
      case _ => ""
    }
  }

  def generateTypeArgs(baseTypes: BaseTypes, typeArgs: Seq[TType]): GeneratorContext = {
    val ctxs = typeArgs.map(getCppTypeName(baseTypes, _))
    val context = mergeGeneratorContexts(ctxs, ", ")
    context.enhance(s"<$context>")
  }

  def generateTypeArgs(generics: Seq[GenericModifier], withTypeName: Boolean = false): String = {
    val templateTypes = generics.map(if (withTypeName) "typename " + _.name else _.name).mkString(", ")
    s"<$templateTypes>"
  }

  def generateFieldDefinition(baseTypes: BaseTypes, f: Field): String = {
    val const = if (f.isMutable) "" else "const "
    s"  $const${generateCppTypeName(baseTypes, f.tpe)} ${f.name}"
  }

  def generateFieldInitializer(handle: FieldInitializerHandle): String = {
    s" = ${handle.content};"
  }

  def generateIncludes(imports: Seq[ImportStmt]): String = {
    val includes = if (imports.isEmpty) ""
    else {
      imports.map(i => {
        val headerFile = getHeaderFileName(i.name)
        s"""#include "$headerFile"""" + "\n"
      }).mkString
    }
    Vector( """#include <memory>""", includes).filter(!_.isEmpty).mkString("\n")
  }

  def generateScopeMethod(methodName: String): String = {
    s"$methodName()"
  }

  val primitiveNames = Map("scala.String" -> "std::string", "scala.Boolean" -> "bool", "scala.Unit" -> "void")

  def getPrimitiveName(primitive: TType): GeneratorContext = {
    val primitiveName: String = primitiveNames.getOrElse(primitive.name, primitive.simpleName.toLowerCase)
    val handles = if (primitive.name == "scala.String") Vector(IncludeHandle("<string>")) else Vector()
    GeneratorContext(primitiveName, handles)
  }

  def mergeGeneratorContexts(seq: Seq[GeneratorContext], seperator: String = "\n",
                             endsWith: String = "", givenContent: String = null): GeneratorContext = {
    val content: String = if (givenContent != null) givenContent
    else seq.filter(_.definedContent).map(expr => expr.content).mkString(seperator) + endsWith // remove empty contents
    val handles: Seq[GeneratorHandle] = seq.flatMap(_.handles)
    GeneratorContext(content, handles)
  }
}
