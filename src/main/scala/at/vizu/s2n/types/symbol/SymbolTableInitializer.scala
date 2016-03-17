package at.vizu.s2n.types.symbol

/**
 * Phil on 16.10.15.
 */
trait SymbolTableInitializer {

  def initScope: TSymbolTable

  def unitType: TType

  def booleanType: TType

  def nullType: TType

  def nothing: TType

  def any: TType

}
