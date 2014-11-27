name := "sbt-plugins"

// WARNING: The following line needed to be early in this file.

sbtPlugin := true


organization := "com.whitepages"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
   "commons-io" % "commons-io" % "2.4",
   "com.eed3si9n" % "sbt-assembly" % "0.11.2" extra("scalaVersion" -> "2.10", "sbtVersion" -> "0.13")  
)

fork in test := true

fork in run := true


