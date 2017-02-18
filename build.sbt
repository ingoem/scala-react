name := "Scala.React"

version := "1.0"

organization := "scala.react"

scalaVersion := "2.9.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")

scalacOptions += "-P:continuations:enable"

libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.0" withSources(),
    "org.joda" % "joda-convert" % "1.2",
    "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0",
    "org.scalatest" %% "scalatest" % "1.7.2" % "test",
    "junit" % "junit" % "4.10" % "test->default",
    "com.novocode" % "junit-interface" % "0.8" % "test->default"
)

parallelExecution := false