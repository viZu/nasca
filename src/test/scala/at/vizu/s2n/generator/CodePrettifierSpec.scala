package at.vizu.s2n.generator

import org.scalatest.{FlatSpec, Matchers}

/**
  * Phil on 02.12.15.
  */
class CodePrettifierSpec extends FlatSpec with Matchers {


  "CodePrettifier" should "return well formatted c++ code" in {

    val r =
      """#include "Point.h"
        |
        |void at_point::Point::__init__class__Point() {
        |
        |}
        |
        |at_point::Point::Point(int x, int y) : x(x), y(y) {
        |  this->x = x;
        |this->y = y;
        |this->__init__class__Point();
        |}
        |
        |int at_point::Point::calc() {
        |  auto calcAcc = [&]() {
        |    return this->x * this->y;
        |  };
        |  while(5 < 7) {
        |    this->h * 6;
        |    this->y - 7;
        |  }
        |  do {
        |    this->x * 8;
        |    this->x * 8;
        |    this->x * 8;
        |  } while (6 < 9);
        |  return calcAcc();;
        |}
        |
        |std::shared_ptr<at_point::Point> at_point::Point::calc2(int i, int j) {
        |  return std::shared_ptr<at_point::Point>(new at_point::Point(this->x * this->y, i * j));;
        |}""".stripMargin
    println(CodePrettifier.prettify(r))
  }


}
