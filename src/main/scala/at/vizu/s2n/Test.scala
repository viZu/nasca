package at.vizu.s2n

/**
  * Phil on 17.12.15.
  */
class Test[-T] {
  // def test(f: Test[T]): Test[T] = f

  def map[U](u: Test[U]): Test[U] = u
}