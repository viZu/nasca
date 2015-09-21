package at.vizu.s2n.args

import java.io.File

import scopt.OptionParser

/**
 * Phil on 21.09.15.
 */
case class Arguments(files: Seq[File] = Seq())