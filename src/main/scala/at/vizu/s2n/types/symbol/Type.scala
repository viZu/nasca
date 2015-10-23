package at.vizu.s2n.types.symbol

import at.vizu.s2n.exception.TypeException

/**
 * Phil on 07.10.15.
 */
case class Type(ctx: Context = Context("", 0), name: String,
                pkg: String = "", mods: Seq[Modifier] = Seq()) extends Modifiable with Nameable {

  private var methods: Seq[Method] = Seq()
  private var fields: Seq[Field] = Seq()
  var parents: Seq[Type] = Seq()
  var generics: Seq[GenericModifier] = Seq()

  override def modifiers: Set[Modifier] = Set() ++ mods

  def findMethod(name: String, args: Seq[Type]): Option[Method] = {
    methods.find(m => m.name == name && m.checkArgs(args)) orElse findMethodInParents(name, args)
  }

  def findMethodInParents(name: String, args: Seq[Type]): Option[Method] = {
    parents.flatMap(_.methods).find(m => m.name == name && m.checkArgs(args))
  }

  def findField(name: String): Option[Field] = {
    fields.find(_.name == name) orElse findFieldInParents(name)
  }

  def findFieldInParents(name: String): Option[Field] = {
    parents.flatMap(_.fields).find(_.name == name)
  }

  def fullClassName = if (pkg.isEmpty) name else pkg + "." + name

  def addMethod(method: Method) = {
    methods = methods :+ method
  }

  private def validateMethod(method: Method) = {
    if (method.isAbstract && !isAbstract) {
      throw new TypeException(method.ctx.fileName, method.ctx.line,
        s"Type $name must either be abstract or implement abstract member ${method.name}")
    }
  }

  def addField(field: Field) = {
    fields = fields :+ field
  }

  def validateField(field: Field) = {
    if (field.isAbstract && !isAbstract) {
      throw new TypeException(field.ctx.fileName, field.ctx.line,
        s"Type $name must either be abstract or implement abstract member ${field.name}")
    }
  }

  def addParent(parent: Type) = {
    parents = parents :+ parent
  }

  def validateParent(p: (Type, Int)) = {
    val (parent, index) = p
    if (index > 0 && !parent.isTrait) {
      throw new TypeException(ctx.fileName, ctx.line, s"Type ${parent.name} needs to be a trait to be mixed in")
    }
  }

  def validate() = {
    methods.foreach(validateMethod)
    fields.foreach(validateField)
    parents.zipWithIndex.foreach(validateParent)
  }
}
