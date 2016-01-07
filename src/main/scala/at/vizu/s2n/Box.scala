package at.vizu.s2n

/**
  * Phil on 07.01.16.
  */
class Box[+T](_x: T) {

  def value(): T = _x

  def map[U](f: T => U): Box[U] = new Box[U](f(value()))

  //def haha(lst: Test[T]) = null

  def box(): Box[T] = new Box[T](value())
}

class Test[+T](x: T)