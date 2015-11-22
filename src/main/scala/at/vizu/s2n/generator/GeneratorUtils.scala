package at.vizu.s2n.generator

import at.vizu.s2n.generator.handles.{FieldInitializerHandle, GeneratorHandle}
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
    simpleTypeName + ".h"
  }

  def getSourceFileName(tpe: TType): String = {
    getSourceFileName(tpe.simpleName)
  }

  def getSourceFileName(simpleTypeName: String): String = {
    simpleTypeName + ".cpp"
  }

  def generateCppTypeName(baseTypes: BaseTypes, tpe: TType): String = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else generateSmartPtr(tpe)
  }

  def getCppTypeName(baseTypes: BaseTypes, tpe: TType): String = {
    if (baseTypes.isPrimitive(tpe)) getPrimitiveName(tpe)
    else getCppTypeName(tpe.pkg, tpe.simpleName)
  }

  def generateSmartPtr(tpe: TType): String = s"${generateSmartPtr(tpe.pkg, tpe.simpleName)}"

  def generateSmartPtr(pkg: String, name: String): String = s"std::shared_ptr<${getCppTypeName(pkg, name)}>"


  def getCppTypeName(pkg: String, name: String): String = {
    if (pkg.isEmpty) name
    else pkg.replaceAll("\\.", "_") + "::" + name
  }

  def generateConstructorDefinition(baseTypes: BaseTypes, m: Method, typeName: String): String = {
    val paramStrings: String = m.params.map(p => getCppTypeName(baseTypes, p.tpe)).mkString(", ")
    s"  $typeName($paramStrings);"
  }

  def generateMethodDefinition(baseTypes: BaseTypes, m: Method): String = {
    val tpeName = generateCppTypeName(baseTypes, m.returnType)
    val params = generateParamsString(baseTypes, m.params)
    s"  $tpeName ${m.name}($params);"
  }

  def generateParamsString(baseTypes: BaseTypes, params: Seq[Param], withVars: Boolean = false) = {
    params.map(p => {
      generateCppTypeName(baseTypes, p.tpe) + (if (withVars) s" ${p.name}" else "")
    }).mkString(", ")
  }

  def generateFieldDefinition(baseTypes: BaseTypes, f: Field): String = {
    val const = if (f.isMutable) "" else "const "
    s"  $const${generateCppTypeName(baseTypes, f.tpe)} ${f.name}"
  }

  def generateFieldInitializer(handle: FieldInitializerHandle): String = {
    s" = ${handle.content};"
  }

  def generateIncludes(imports: Seq[ImportStmt]): String = {
    if (imports.isEmpty) ""
    else {
      imports.map(i => {
        val headerFile = getHeaderFileName(i.name)
        s"""#include "$headerFile"""" + "\n"
      }).mkString
    }
  }

  def generateScopeMethod(methodName: String): String = {
    s"$methodName()"
  }

  val primitiveNames = Map("String" -> "std::string", "Boolean" -> "bool", "Unit" -> "void")

  def getPrimitiveName(primitive: TType): String = {
    primitiveNames.getOrElse(primitive.simpleName, primitive.simpleName.toLowerCase)
  }

  def mergeGeneratorContexts(seq: Seq[GeneratorContext], seperator: String = "\n", endsWith: String = ""): GeneratorContext = {
    val content: String = seq.filter(_.definedContent).map(_.content).mkString(seperator) + endsWith // remove empty contents
    val handles: Seq[GeneratorHandle] = seq.flatMap(_.handles)
    GeneratorContext(content, handles)
  }
}
