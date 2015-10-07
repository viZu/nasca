package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
class Scope(var parent: Scope = null) {

  private var types: Seq[Type] = Vector()
  private var identifiers: Seq[Identifier] = Vector()

  def enterScope(): Scope = new Scope(this)

  def exitScope(): Scope = {
    val parent = this.parent

    // cleanup for garbage collector ?
    this.parent = null
    this.types = null
    this.identifiers = null
    parent
  }

  def add(tpe: Type) = types = types :+ tpe

  def add(identifier: Identifier) = identifiers = identifiers :+ identifier

  def findIdentifier(name: String): Option[Identifier] = identifiers.find(_.name == name)

  def findType(name: String): Option[Type] = types.find(_.fullClassName == name)

  def checkScope(identifier: Identifier) = findIdentifier(identifier.name).isEmpty

  def checkScope(tpe: Type) = findType(tpe.fullClassName).isEmpty
}
