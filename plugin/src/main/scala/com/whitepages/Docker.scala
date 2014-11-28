package com.whitepages

import sbt._
import sbt.Keys._
import java.io.{PrintWriter, File}
import sbt.File
import org.apache.commons.io.FileUtils
import sbtassembly.Plugin._
import AssemblyKeys._

object Docker extends Plugin {

  val dockerRepo = SettingKey[String]("dockerRepo", "repo for docker images")

  val maintainer = SettingKey[String]("maintainer", "email of service maintainer")

  val dockerBase = SettingKey[String]("dockerBase", "base repo for docker images")

  val dockerBaseDefault = dockerBase := "base/scala-base:1.0.0"

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

  val doDocker = SettingKey[Boolean]("doDocker", "Release will push image to docker repo")

  val doDockerDefault = doDocker := false

  val doJar = SettingKey[Boolean]("doJar", "Release will push jars to artifactory")

  val doJarDefault = doJar := true

  val dockerGenTask = dockerGen := {
    // depends on assembly task
    assembly.value
    val v = version.value
    val n = name.value
    val sc = serviceClass.value
    val p = servicePort.value
    val opt = dockerJavaOptions.value
    val base = dockerBase.value
    val main = maintainer.value
    val dr = dockerRepo.value
    val p1 = p + 30000
    val scalaVer = scalaVersion.value
    val scalaVer2 = scalaVer.split("[.]").take(2).mkString(".")
    val f0: File = new File("target/docker")
    f0.delete()
    val f1: File = new File("target/docker/files")
    f1.mkdir()
    FileUtils.copyFileToDirectory(new File(s"target/scala-$scalaVer2/$n-assembly-$v.jar"), f1)
    val f: File = new File("target/docker/Dockerfile")
    val out = new PrintWriter(f)
    out.println(s"FROM $dr/$base")
    out.println(s"MAINTAINER $main")
    out.println(s"COPY files /opt/wp/$n")
    out.println(s"WORKDIR /opt/wp/$n")
    out.println(s"EXPOSE $p")
    out.println(s"EXPOSE $p1")
    val opts = Seq("java", "-cp", s"$n-assembly-$v.jar") ++ opt ++ Seq("com.whitepages.framework.service.DockerRunner", s"$sc")
    val fopt = opts.mkString("[\"", "\",\"", "\"]")
    out.println(s"CMD $fopt")
    out.close
    println("Generated " + f.getPath)
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
      dockerGroupDefault, dockerJavaOptionsDefault, dockerPushDefault, dockerBaseDefault)

}

