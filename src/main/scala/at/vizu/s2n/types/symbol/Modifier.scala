package at.vizu.s2n.types.symbol

/**
 * Phil on 16.10.15.
 */
trait Modifier

case class Private() extends Modifier

case class PackagePrivate() extends Modifier

case class Protected() extends Modifier

case class Trait() extends Modifier

case class Abstract() extends Modifier

case class Override() extends Modifier

case class Sealed() extends Modifier

case class Case() extends Modifier

trait Modifiable {
  def modifiers: Set[Modifier]

  def isPrivate = modifiers.contains(Private())

  def isPackagePrivate = modifiers.contains(PackagePrivate())

  def isProtected = modifiers.contains(Protected())

  def isPublic = !isPrivate && !isPackagePrivate && !isProtected

  def isTrait = modifiers.contains(Trait())

  def isAbstract = modifiers.contains(Abstract())

  def isSealed = modifiers.contains(Sealed())

  def isCase = modifiers.contains(Case())
}