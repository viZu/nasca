package at.vizu.s2n.generator.handles

import at.vizu.s2n.types.symbol.Method

/**
  * Phil on 18.11.15.
  */
trait GeneratorHandle {
  def key: String

  def content: String
}

case class FieldInitializerHandle(fieldName: String, content: String) extends GeneratorHandle {
  def key = fieldName
}

case class MethodDefinitionHandle(method: Method) extends GeneratorHandle {
  def content = ""

  def key = method.name
}

case class MethodHandle(content: String) extends GeneratorHandle {

  def key = ""
}

case class IncludeHandle(cppInclude: String) extends GeneratorHandle {
  def key = cppInclude

  def content = s"#import $cppInclude"
}