package at.vizu.s2n.generator


/**
  * Phil on 20.11.15.
  */
package object expression {

  implicit def pathElementToPath(pathElement: Expression): Path = {
    Seq(pathElement)
  }

}