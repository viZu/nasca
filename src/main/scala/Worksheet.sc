class Foo(i: Int) {

  def this(i: String, i2: Int) = {
    this(i2)
  }

}

class Bar5(i: Int) extends Foo("Hello", i)