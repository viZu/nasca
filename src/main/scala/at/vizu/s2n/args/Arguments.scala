package at.vizu.s2n.args

import java.nio.file.{Path, Paths}

/**
 * Phil on 21.09.15.
 */
case class Arguments(env: String = "",
                     files: Seq[Path] = Vector(),
                     out: Path = Paths.get(""),
                     main: String = "",
                     binName: String = "binary")