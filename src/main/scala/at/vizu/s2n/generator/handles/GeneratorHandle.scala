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

sealed trait IncludeWrapperStrategy {
  def wrapInclude(include: String): String
}

case object AngleWrapper extends IncludeWrapperStrategy {
  override def wrapInclude(include: String): String = s"<$include>"
}

case object QuotationWrapper extends IncludeWrapperStrategy {
  override def wrapInclude(include: String): String = s""""$include""""
}

case class IncludeHandle(cppInclude: String, wrapIn: IncludeWrapperStrategy) extends GeneratorHandle {
  def key = cppInclude

  def content = s"#include ${wrapIn.wrapInclude(cppInclude)}"
}