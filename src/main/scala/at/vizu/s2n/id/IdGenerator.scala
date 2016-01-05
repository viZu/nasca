package at.vizu.s2n.id

import java.util.UUID

/**
  * Phil on 02.01.16.
  */
object IdGenerator {

  def generateId() = UUID.randomUUID().toString

}
