package at.vizu.s2n.types.symbol

/**
  * Phil on 07.12.15.
  */
class AppliedGenericType(appliedTypes: Seq[TType], newMethods: Seq[Method], newFields: Seq[Field],
                         genericType: GenericType) extends GenericType(
  genericType.ctx, genericType.simpleName, genericType.pkg, genericType.mods, Seq(), genericType.isObject) {

  override def methods: Seq[Method] = newMethods

  override def fields: Seq[Field] = newFields

  override private[symbol] def parents: Seq[TType] = genericType.parents

}
