package at.vizu.s2n.generator

import at.vizu.s2n.generator.handles.GeneratorHandle

/**
  * Phil on 18.11.15.
  */
case class GeneratorContext(content: String = "", handles: Set[GeneratorHandle] = Set()) {

  def isEmpty = emptyContent && handles.isEmpty

  def isNonEmpty = !isEmpty

  def emptyContent = content.trim.isEmpty

  def definedContent = !emptyContent

  def removeContent() = GeneratorContext(handles = this.handles)

  def removeContent(handlesToAdd: Set[GeneratorHandle]) = GeneratorContext(handles = this.handles ++ handlesToAdd)

  def enhance(content: String, handles: Set[GeneratorHandle] = Set()) = GeneratorContext(content, this.handles ++ handles)

  def ++(handles: Set[GeneratorHandle]) = GeneratorContext(content, this.handles ++ handles)

  def +(handle: GeneratorHandle): GeneratorContext = GeneratorContext(content, this.handles + handle)

  def +(optHandle: Option[GeneratorHandle]): GeneratorContext = optHandle match {
    case None => this
    case Some(handle) => this + handle
  }

  def +(content: String) = GeneratorContext(this.content + content, handles)

  def removeHandles[T <: GeneratorHandle](clazz: Class[T]) = {
    val newHandles = this.handles.filter(_.getClass != clazz)
    GeneratorContext(content, newHandles)
  }

  override def toString: String = content
}
