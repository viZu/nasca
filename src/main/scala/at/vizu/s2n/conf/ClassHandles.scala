package at.vizu.s2n.conf

import at.vizu.s2n.generator.GeneratorContext
import at.vizu.s2n.generator.handles.IncludeHandle
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

import scala.collection.mutable.ArrayBuffer

/**
  * Phil on 01.01.16.
  */
class ClassRenamingHandle {

  private[conf] var matcher: TType => Boolean = null
  private[conf] var renamer: (BaseTypes, TType) => GeneratorContext = null
  private[conf] var includeHandle: IncludeHandle = null

  def withMatcher(f: TType => Boolean): Unit = matcher = f


  def withRename(f: (BaseTypes, TType) => GeneratorContext): Unit = renamer = f


  def withIncludeHandle(i: IncludeHandle) = includeHandle = i
}

class ClassHandlesConfig {

  private val renamingHandles = ArrayBuffer[ClassRenamingHandle]()

  private[conf] def addClassRenamingHandle(h: ClassRenamingHandle) = {
    validateRenamingHandle(h)
    renamingHandles += h
  }

  private def validateRenamingHandle(h: ClassRenamingHandle) = {
    if (h.matcher == null || h.renamer == null) {
      throw new RuntimeException(s"ClassRenamingHandle is not valid")
    }
  }

  def hasIncludeHandle(tpe: TType) = getIncludeHandle(tpe) != null

  def getIncludeHandle(tpe: TType) = findRenamingHandle(tpe) match {
    case None => null
    case Some(r) => r.includeHandle
  }

  def findRenamingHandle(tpe: TType) = renamingHandles.find(_.matcher(tpe))

  def hasRenamingHandle(tpe: TType) = findRenamingHandle(tpe).isDefined

  def getRenamingHandle(tpe: TType) = {
    renamingHandles.find(_.matcher(tpe)) match {
      case None => null
      case Some(h) => h.renamer
    }
  }

}
