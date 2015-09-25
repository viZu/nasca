package at.vizu.s2n.args

import java.io.File
import java.nio.file.Path

/**
 * Phil on 21.09.15.
 */
case class Arguments(env: String = "", files: Seq[Path] = Seq(), out: File = new File("."))