package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
case class GenericType(_ctx: Context, genericName: String) extends TType {
  override def ctx: Context = _ctx

  override def isObject: Boolean = false

  override def methods: Seq[Method] = Seq()

  override def pkg: String = ""

  override def simpleName: String = genericName

  override def hasParent(tpe: TType): Boolean = ???

  override def fields: Seq[Field] = Seq()

  override def validate(): Unit = ???

  override def findField(execCtx: TType, name: String): Option[Field] = ???

  override def mods: Seq[Modifier] = ???

  override protected def parents: Seq[TType] = ???

  override def findMethod(execCtx: TType, name: String, args: Seq[TType]): Option[Method] = ???
}
