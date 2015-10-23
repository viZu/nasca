package at.vizu.s2n.types.symbol.util


import at.vizu.s2n.types.symbol._

import scala.reflect.runtime.universe._

/**
 * Phil on 21.10.15.
 */
object TypeUtils {

  def getModifiers(mods: Modifiers): Seq[Modifier] = {
    var modifiers: List[Modifier] = List[Modifier]()
    val flagSet = mods.flags
    val z = (flagSet | Flag.DEFAULTPARAM) == Flag.DEFAULTPARAM
    if (isZero(flagSet)) {
      modifiers
    } else {
      if (mods.privateWithin.toString != "") {
        //TODO which package?
        modifiers = PackagePrivate() :: modifiers
      }
      if (mods.hasFlag(Flag.PRIVATE)) {
        modifiers = Private() :: modifiers
      }
      if (mods.hasFlag(Flag.PROTECTED)) {
        modifiers = Protected() :: modifiers
      }
      if (mods.hasFlag(Flag.ABSTRACT)) {
        modifiers = Abstract() :: modifiers
      }
      if (mods.hasFlag(Flag.DEFERRED)) {
        modifiers = Abstract() :: modifiers
      }
      if (mods.hasFlag(Flag.SEALED)) {
        modifiers = Sealed() :: modifiers
      }
      if (mods.hasFlag(Flag.CASE)) {
        modifiers = Case() :: modifiers
      }
      if (mods.hasFlag(Flag.OVERRIDE)) {
        modifiers = Override() :: modifiers
      }
      if (mods.hasFlag(Flag.TRAIT)) {
        modifiers = modifiers
      }
      modifiers
    }
  }

  private def isZero(flags: FlagSet): Boolean = {
    flags == 0
  }

}
