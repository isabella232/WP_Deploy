import sbt._
import Keys._

name := "sample-app"

//organization := "com.whitepages"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.whitepages" %% "scala-webservice" % "9.4.3",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test"
)