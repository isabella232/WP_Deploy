package com.whitepages.platformCommon.data

import com.persist.JsonOps._
import com.whitepages.framework.util.ClassSupport

case class JsonServiceData(j: Json) extends ServiceData with ClassSupport {
  val svcs = jgetArray(j, "services")

  private def getService(svcName: String): JsonObject = {
    val select = svcs filter {
      case svc =>
        jgetString(svc, "service") == svcName
    }
    jgetObject(select.headOption.getOrElse(emptyJsonObject))
  }


  private def getEnvironment(svc: String, envName: String): JsonObject = {
    val envs = jgetArray(getService(svc), "envs")
    val select = envs filter {
      case env => jgetString(env, "env") == envName
    }
    jgetObject(select.headOption.getOrElse(emptyJsonObject))
  }

  private def getServer(svc: String, env: String, svrName: String): JsonObject = {
    val svrs = jgetArray(getEnvironment(svc, env), "servers")
    val select = svrs filter {
      case svr => jgetString(svr, "server") == svrName
    }
    jgetObject(select.headOption.getOrElse(emptyJsonObject))
  }

  // list of all service names
  override def services(): Seq[String] = {
    svcs map {
      case svc => jgetString(svc, "service")
    }
  }

  // return info about an environment
  override def instance(service: String, environment: String): JsonObject = {
    val env = getEnvironment(service, environment)
    jgetObject(jdelete(env, "servers"))
  }

  // list of all environments for a service
  override def instances(service: String): Seq[String] = {
    val envs = jgetArray(getService(service), "envs")
    envs map {
      case env => jgetString(env, "env")
    }
  }

  // return info about a server
  override def server(service: String, environment: String, sub:String, server: String): JsonObject = {
    getServer(service, environment, server)
  }

  // return info about a service
  override def service(service: String): JsonObject = {
    val svc = getService(service)
    jgetObject(jdelete(svc,"envs"))
  }

  // list of all servers for a service:environment
  override def servers(service: String, environment: String, sub:String): Seq[String] = {
    val svrs = jgetArray(getEnvironment(service, environment), "servers")
    svrs map {
      case svr => jgetString(svr, "server")
    }
  }

  // list of all environment names
  override def environments(): Seq[String] = ???

  // return info about a service
  override def environment(environment: String): JsonObject = ???

  // define a new service
  override def defineService(service: String, info: JsonObject): Unit = ???

  // define a new environment
  override def defineEnvironment(environment: String, info: JsonObject): Unit = ???

  // define a new service instance
  override def defineInstance(service: String, environment: String, info: JsonObject): Unit = ???

  // define a new a server
  def defineServer(service: String, environment: String, sub:String, server: String, info: JsonObject): Unit = ???

  // list all subs
  override def subs(service:String, environment:String): Seq[String] = ???

  // return info about a sub
  override def sub(service: String, environment: String, sub: String): JsonObject = ???

  // define a new sub
  override def defineSub(service: String, environment: String, sub: String, info: JsonObject): Unit = ???

  // update an existing sub
  def updateSub(service: String, environment: String, sub: String, info: JsonObject): Unit = ???
}
