import sbt._
import Keys._

name := "service-agent"

organization := "com.whitepages"

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.11.1")   // sbt-release bug!

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, javaSource, organization)

buildInfoPackage := "com.whitepages.info." + (name.value.replace("-", "_"))

libraryDependencies ++= Seq(
  "com.whitepages" %% "scala-webservice" % "9.6.1-SNAPSHOT"
)



