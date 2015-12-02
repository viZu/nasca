package at.vizu.s2n.generator


/**
  * Phil on 20.11.15.
  */
package object expression {

  implicit def pathElementToPath(pathElement: Expression): Path = {
    Seq(pathElement)
  }

  val operatorMapping = Map("\\$plus" -> "+", "\\$less" -> "<", "\\$greater" -> ">", "\\$band" -> "!", "$eq" -> "=",
    "\\$times" -> "*", "\\$amp" -> "&", "\\$bar" -> "|", "\\$div" -> "/", "\\$minus" -> "-",
    "\\$percent" -> "%")

  def prettifyOperator(op: String) = {
    operatorMapping.foldLeft(op) {
      case (tmp, (from, to)) => tmp.replaceAll(from, to)
    }
  }
}
