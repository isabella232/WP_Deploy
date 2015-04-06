package com.whitepages

import sbt._
import sbt.Keys._
import java.io.PrintWriter
import sbt.File
import org.apache.commons.io.FileUtils
import sbtassembly.AssemblyKeys._

object Docker extends Plugin {

  val dockerLocalOverride = SettingKey[Boolean]("dockerLocalOverride", "use local DockerFile")

  val dockerLocalOverrideDefault = dockerLocalOverride := false

  val dockerRepo = SettingKey[String]("dockerRepo", "repo for docker images")

  val maintainer = SettingKey[String]("maintainer", "email of service maintainer")

  val dockerBase = SettingKey[String]("dockerBase", "base repo for docker images")

  val dockerBaseDefault = dockerBase := "platform/scala-base:1.0.0"

  val serviceClass = SettingKey[String]("serviceClass", "top level service class")

  val serviceClassDefault = serviceClass := ""

  val servicePort = SettingKey[Int]("servicePort", "service REST port")

  val servicePortDefault = servicePort := 0

  val dockerGroup = SettingKey[String]("dockerGroup", "team")

  val dockerGroupDefault = dockerGroup := ""

  val dockerJavaOptions = SettingKey[Seq[String]]("dockerJavaOption", "docker run jvm options")

  val dockerJavaOptionsDefault = dockerJavaOptions := Seq()

  val dockerGen = TaskKey[Unit]("dockerGen", "Generate docker file")

  val docker = TaskKey[Unit]("docker", "Push image to docker repo")

  val dockerPush = SettingKey[Boolean]("dockerPush", "Push image to repo")

  val dockerPushDefault = dockerPush := true

  val dockerExtraPorts = SettingKey[Seq[Int]]("dockerExtraPorts", "extra ports to expose")

  val dockerExtraPortsDefault = dockerExtraPorts := Seq[Int]()

  val dockerGenTask = dockerGen := {
    assembly.value
    val v = version.value
    val n = name.value
    val sc = serviceClass.value
    val p = servicePort.value
    val base = dockerBase.value
    val main = maintainer.value
    val dr = dockerRepo.value
    val p1 = p + 30000
    val pJMX = p + 10000
    val scalaVer = scalaVersion.value
    val scalaVer2 = scalaVer.split("[.]").take(2).mkString(".")
    val f0: File = new File("target/docker")
    f0.delete()
    val f1: File = new File("target/docker/files")
    f1.mkdir()
    FileUtils.copyFileToDirectory(new File(s"target/scala-$scalaVer2/$n.jar"), f1)
    val f: File = new File("target/docker/Dockerfile")
    if (dockerLocalOverride.value) {
      val local = new File("DockerLocal/Dockerfile")
      sbt.IO.copy(Seq((local,f)),overwrite = true)
    } else {
      val out = new PrintWriter(f)
      out.println(s"FROM $dr/$base")
      out.println(s"MAINTAINER $main")
      out.println(s"COPY files /opt/wp/$n")
      out.println(s"WORKDIR /opt/wp/$n")
      out.println(s"EXPOSE $p")
      out.println(s"EXPOSE $p1")
      out.println(s"EXPOSE $pJMX")
      for (port <- dockerExtraPorts.value) {
        out.println(s"EXPOSE $port")
      }
      val enableRemoteJMX = Seq("-Dcom.sun.management.jmxremote.authenticate=false", "-Dcom.sun.management.jmxremote.ssl=false",
        "-Dcom.sun.management.jmxremote", s"-Dcom.sun.management.jmxremote.port=$pJMX")
      val javaHeapDumpSettings = Seq("-XX:+HeapDumpOnOutOfMemoryError", "-XX:HeapDumpPath=/tmp") //TODO: mount a filestyem from CoreOS that is durable
      val opt = javaHeapDumpSettings ++ enableRemoteJMX ++ dockerJavaOptions.value
      val opts = Seq("java", "-cp", s"$n.jar") ++ opt ++ Seq("com.whitepages.framework.service.DockerRunner", s"$sc")
      val fopt = opts.mkString("[\"", "\",\"", "\"]")
      out.println(s"CMD $fopt")
      out.close()
      println("Generated " + f.getPath)
    }
  }

  // invoke from release if doDocker
  val dockerTask = docker := {
    // depends on dockerGen
    dockerGen.value
    val v = version.value
    val n = name.value
    val g = dockerGroup.value
    val dr = dockerRepo.value
    println(s"docker build -t $dr/$g/$n:$v target/docker")
    val r1 = Process("docker", Seq("build", "-t", s"$dr/$g/$n:$v", "target/docker")).!!
    println(r1)
    if (dockerPush.value) {
      println(s"docker push $dr/$g/$n:$v")
      val r2 = Process("docker", Seq("push", s"$dr/$g/$n:$v")).#>(System.out).!!
    }
    println("Docker Done")
  }

  lazy val dockerSettings: Seq[Def.Setting[_]] =
    Seq(dockerGenTask, dockerTask, serviceClassDefault, servicePortDefault,
      dockerGroupDefault, dockerJavaOptionsDefault, dockerPushDefault, dockerBaseDefault,
    dockerLocalOverrideDefault, dockerExtraPortsDefault)

}

