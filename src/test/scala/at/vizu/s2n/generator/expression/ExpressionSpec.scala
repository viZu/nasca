package at.vizu.s2n.generator.expression

import org.scalatest.{FlatSpec, Matchers}

/**
  * Phil on 02.12.15.
  */
class ExpressionSpec extends FlatSpec with Matchers {

  "prettifyOperators" should "return '+' for string '$plus'" in {
    prettifyOperator("$plus") should be("+")
  }

  "prettifyOperators" should "return '<' for string '$less'" in {
    prettifyOperator("$less") should be("<")
  }
}
