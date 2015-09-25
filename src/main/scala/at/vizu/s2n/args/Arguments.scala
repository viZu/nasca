package at.vizu.s2n.args

import java.nio.file.{Path, Paths}

/**
 * Phil on 21.09.15.
 */
case class Arguments(env: String = "", files: Seq[Path] = Seq(), out: Path = Paths.get(""))