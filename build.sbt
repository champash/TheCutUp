name := "TheCutUp"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.9",
  "com.typesafe.akka" %% "akka-http" % "10.2.9")
