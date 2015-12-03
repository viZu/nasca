package at.vizu.s2n.types.symbol

/**
  * Phil on 03.12.15.
  */
trait Member extends Nameable with Modifiable {

  def tpe: TType

}
