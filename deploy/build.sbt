import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

name := "service-agent"

organization := "com.whitepages"

scalaVersion := "2.11.1"

assemblySettings

jarName in assembly := "service-deploy.jar"

libraryDependencies ++= Seq(
  "jline" % "jline" % "2.12",
  "com.typesafe.akka" %% "akka-actor" % "2.3.2",
  "com.typesafe.akka" %% "akka-remote" % "2.3.2",
  "com.persist" % "persist-json_2.10" % "0.16",
  "joda-time" % "joda-time" % "2.2",
  "org.joda" % "joda-convert" % "1.2"
)



