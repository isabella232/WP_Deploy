import sbt._
import Keys._
//import sbtassembly.Plugin.AssemblyKeys._

name := "sample-app"

scalaVersion := "2.11.1"

sbtassembly.Plugin.assemblySettings

dockerSettings

serviceClass := "com.whitepages.sampleApp.SampleMain"

servicePort := 8900

maintainer := "you@foo.com"

dockerRepo := "myrepo.foo.com"

dockerGroup := "test"

dockerPush := false

libraryDependencies ++= Seq(
  "com.whitepages" %% "scala-webservice" % "9.4.10-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test"
)