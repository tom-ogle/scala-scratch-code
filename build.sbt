

name := "scala-scratch-code"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "com.typesafe.akka" %% "akka-actor" % "2.4.0",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.specs2" %% "specs2-core" % "3.8.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
