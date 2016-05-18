

name := "scala-scratch-code"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "com.typesafe.akka" %% "akka-actor" % "2.4.0",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
