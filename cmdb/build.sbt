name := "cmdb"

organization := "com.whitepages"

//repo := "search-dev"

scalaVersion := "2.11.1"

//wpSettings

seq(webSettings :_*)

libraryDependencies ++=Seq(
  "org.eclipse.jetty" % "jetty-webapp" % "9.1.0.v20131115" % "container",
  "org.eclipse.jetty" % "jetty-plus"   % "9.1.0.v20131115" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
  "com.vaadin" % "vaadin-server" % "7.1.9",
  "com.vaadin" % "vaadin-client-compiled" % "7.1.9",
  "com.vaadin" % "vaadin-themes" % "7.1.9",
  "com.whitepages" %% "scala-webservice" % "9.4.10-SNAPSHOT"
  //"com.whitepages" % "scala-test_2.10" % "7.0.1" % "test"
)



