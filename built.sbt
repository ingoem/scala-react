name := "scala-react"

organization := "scala"

version := "1.0"

scalaVersion := "2.10.0"

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0")

scalacOptions ++= Seq(
	"-deprecation",
	"-unchecked",
	"-P:continuations:enable"
)

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.0.M6-SNAP8" % "test",
	"junit" % "junit" % "4.10" % "test"
)
