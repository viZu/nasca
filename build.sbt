import com.trueaccord.scalapb.{ScalaPbPlugin => PB}
import sbt.Keys._

name := "scala-to-native"

version := "1.0"

scalaVersion := "2.11.7"
val scoptVersion = "3.3.0"
val scalaTestVersion = "2.2.4"
val macwireVersion = "1.0.1"
val scalaLoggingVersion = "3.1.0"
val logbackVersion = "1.1.3"
val scalaIOVersion = "0.4.3"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.github.scopt" %% "scopt" % scoptVersion
libraryDependencies += "org.scalatest" % "scalatest_2.11" % scalaTestVersion % "test"
libraryDependencies += "com.softwaremill.macwire" %% "macros" % macwireVersion
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion
libraryDependencies += "ch.qos.logback" % "logback-classic" % logbackVersion
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % scalaIOVersion
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % scalaIOVersion

assemblyJarName in assembly := "s2n.jar"
test in assembly := {}
mainClass in assembly := Some("at.vizu.s2n.Main")

PB.protobufSettings