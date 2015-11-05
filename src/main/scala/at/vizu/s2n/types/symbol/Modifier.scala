package at.vizu.s2n.types.symbol

/**
 * Phil on 16.10.15.
 */
trait Modifier

object Private extends Modifier

object PackagePrivate extends Modifier

object Protected extends Modifier

object Trait extends Modifier

object Abstract extends Modifier

object Override extends Modifier

object Sealed extends Modifier

object Final extends Modifier

object Case extends Modifier

object Mutable extends Modifier

trait Modifiable {
  val modifiers: Set[Modifier]

  def isPrivate = modifiers.contains(Private)

  def isPackagePrivate = modifiers.contains(PackagePrivate)

  def isProtected = modifiers.contains(Protected)

  def isPublic = !isPrivate && !isPackagePrivate && !isProtected

  def isTrait = modifiers.contains(Trait)

  def isAbstract = modifiers.contains(Abstract)

  def isOverride = modifiers.contains(Override)

  def isSealed = modifiers.contains(Sealed)

  def isFinal = modifiers.contains(Final)

  def isCase = modifiers.contains(Case)

  def isMutable = modifiers.contains(Mutable)
}