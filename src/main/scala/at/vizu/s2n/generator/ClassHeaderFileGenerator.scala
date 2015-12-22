package at.vizu.s2n.generator

import at.vizu.s2n.types.result.{ClassImplementation, ImportStmt}
import at.vizu.s2n.types.symbol.{BaseTypes, TType}

/**
  * Phil on 12.11.15.
  */
class ClassHeaderFileGenerator(_baseTypes: BaseTypes, _packageName: String,
                               _imports: Seq[ImportStmt], implementation: ClassImplementation) extends HeaderFileGenerator {

  override protected def selfType: TType = implementation.tpe

  override protected def baseTypes: BaseTypes = _baseTypes

  override protected def packageName: String = _packageName
}
