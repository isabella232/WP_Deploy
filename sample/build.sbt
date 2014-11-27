import sbt._
import Keys._

name := "sample-svc"

organization := "com.whitepages"

repo := "search-dev"

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.11.1")   // sbt-release bug!

wpSettings

libraryDependencies ++= Seq(
  "com.whitepages" %% "sample-svc-thrift" % "0.1.1",
  "com.whitepages" %% "scala-webservice" % "9.4.3",
  "com.whitepages" %% "scala-test" % "9.0.0" % "test"
)



