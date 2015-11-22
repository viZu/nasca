package at.vizu.s2n

import at.vizu.s2n.generator.path.Expression

/**
  * Phil on 20.11.15.
  */
package object generator {

  type Path = Seq[Expression]

  implicit def stringToGeneratorContext(str: String): GeneratorContext = GeneratorContext(str)

  def generateExpressionChain(path: Path, seperator: String = "", endsWith: String = ""): GeneratorContext = {
    GeneratorUtils.mergeGeneratorContexts(path.map(_.generate), "")
  }

}
