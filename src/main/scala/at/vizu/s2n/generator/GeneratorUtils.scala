package at.vizu.s2n.generator

import at.vizu.s2n.types.result.ImportStmt
import at.vizu.s2n.types.symbol.{Field, Method, TType, TypeUtils}

/**
  * Phil on 11.11.15.
  */
object GeneratorUtils {

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

  def generateCppTypeName(tpe: TType): String = {
    if (TypeUtils.isPrimitive(tpe)) getPrimitiveName(tpe)
    else generateSmartPtr(tpe)
  }

  def generateSmartPtr(tpe: TType): String = s"${generateSmartPtr(tpe.pkg, tpe.simpleName)}"

  def generateSmartPtr(pkg: String, name: String): String = s"std::shared_ptr<${getCppTypeName(pkg, name)}>"

  def getCppTypeName(tpe: TType): String = {
    if (TypeUtils.isPrimitive(tpe)) getPrimitiveName(tpe)
    else getCppTypeName(tpe.pkg, tpe.simpleName)
  }

  def getCppTypeName(pkg: String, name: String): String = {
    if (pkg.isEmpty) name
    else pkg.replaceAll("\\.", "_") + "::" + name
  }

  def generateConstructorDefinition(m: Method, typeName: String): String = {
    val paramStrings: String = m.params.map(p => getCppTypeName(p.tpe)).mkString(", ")
    s"  $typeName($paramStrings);"
  }

  def generateMethodDefinition(m: Method): String = {
    val tpeName = generateCppTypeName(m.returnType)
    val params = m.params.map(p => generateCppTypeName(p.tpe)).mkString(", ")
    s"  $tpeName ${m.name}($params);"
  }

  def generateFieldDefinition(f: Field): String = {
    s"  ${generateCppTypeName(f.tpe)} ${f.name};"
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

  val primitiveNames = Map("String" -> "std::string", "Boolean" -> "bool", "Unit" -> "void")

  def getPrimitiveName(primitive: TType): String = {
    primitiveNames.getOrElse(primitive.simpleName, primitive.simpleName.toLowerCase)
  }
}
