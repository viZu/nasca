import sbt.Keys._

name := "scala-to-native"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "com.softwaremill.macwire" %% "macros" % "1.0.1"

assemblyJarName in assembly := "s2n.jar"

mainClass in assembly := Some("at.vizu.s2n.Main")