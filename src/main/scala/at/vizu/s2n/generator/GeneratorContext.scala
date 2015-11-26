package at.vizu.s2n.generator

import at.vizu.s2n.generator.handles.GeneratorHandle

/**
  * Phil on 18.11.15.
  */
case class GeneratorContext(content: String = "", handles: Seq[GeneratorHandle] = Seq()) {

  def isEmpty = emptyContent && handles.isEmpty

  def isNonEmpty = !isEmpty

  def emptyContent = content.trim.isEmpty

  def definedContent = !emptyContent

  def removeContent() = GeneratorContext(handles = this.handles)

  def removeContent(handlesToAdd: Seq[GeneratorHandle]) = GeneratorContext(handles = this.handles ++ handlesToAdd)

  def enhance(content: String, handles: Seq[GeneratorHandle] = Seq()) = GeneratorContext(content, this.handles ++ handles)

  def enhance(handles: Seq[GeneratorHandle]) = GeneratorContext(content, this.handles ++ handles)

  def enhance(handle: GeneratorHandle) = GeneratorContext(content, this.handles :+ handle)

}
