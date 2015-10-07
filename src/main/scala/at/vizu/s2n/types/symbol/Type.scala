package at.vizu.s2n.types.symbol

/**
 * Phil on 07.10.15.
 */
case class Type(name: String, pkg: String = "", methods: Seq[Method] = Seq(),
                fields: Seq[Field] = Seq(), superClasses: Seq[Type] = Seq(), generics: Seq[GenericModifier] = Seq()) {

  def findMethod(name: String): Option[Method] = {
    methods.find(_.name == name)
  }

  def findField(name: String): Option[Field] = {
    fields.find(_.name == name)
  }

  def fullClassName = pkg + "." + name

}
