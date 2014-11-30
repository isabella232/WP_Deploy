import sbt._
import Keys._
//import sbtassembly.Plugin.AssemblyKeys._

name := "sample-app"

scalaVersion := "2.11.1"

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, javaSource, organization)

buildInfoPackage := "com.whitepages.info." + (name.value.replace("-", "_"))

sbtassembly.Plugin.assemblySettings

dockerSettings

serviceClass := "com.whitepages.sampleApp.SampleMain"

servicePort := 8888

maintainer := "you@foo.com"

dockerRepo := "myrepo.foo.com"

dockerGroup := "test"

dockerPush := false

libraryDependencies ++= Seq(
  "com.whitepages" %% "scala-webservice" % "9.4.10-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test"
)