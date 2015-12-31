package at.vizu.s2n.args

import java.nio.file.{Path, Paths}

import at.vizu.s2n.log.{LogLevel, Warn}

/**
 * Phil on 21.09.15.
 */
case class Arguments(env: String = "",
                     files: Seq[Path] = Vector(),
                     out: Path = Paths.get(""),
                     main: String = "",
                     binName: String = "binary",
                     logLevel: LogLevel = Warn,
                     stdout: Option[String] = None,
                     stderr: Option[String] = None)