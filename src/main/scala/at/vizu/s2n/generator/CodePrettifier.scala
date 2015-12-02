package at.vizu.s2n.generator

/**
  * Phil on 02.12.15.
  */
object CodePrettifier {

  def prettify(content: String): String = {
    var intendLevel = 0
    content.split("\n").foldLeft("")((tmp, line) => {
      val trimmedLine = line.trim
      val (tmpIntendLevel, newLineCount) = if (trimmedLine.endsWith("{") && trimmedLine.startsWith("}")) {
        (intendLevel - 1, 1)
      } else if (trimmedLine.endsWith("{")) {
        intendLevel += 1
        (intendLevel - 1, 1)
      } else if (trimmedLine.startsWith("}")) {
        intendLevel -= 1
        (intendLevel, if (intendLevel == 0) 1 else 1)
      } else {
        (intendLevel, 1)
      }

      tmp + "  " * tmpIntendLevel + trimmedLine + "\n" * newLineCount
    })
  }
}
