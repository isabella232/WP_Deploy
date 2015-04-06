package com.whitepages.platformCommon.data

import com.persist.JsonOps.JsonObject
import com.whitepages.framework.exceptions.BadInputException
import EtcdServiceData._

object EtcdServiceData {
  val svcKey = "SVC"
  val envKey = "ENV"
  val serverKey = "SVR"
  val subKey = "SUB"
}

case class EtcdServiceData() extends ServiceData  {

  lazy val client = EtcdClient() // lazy to facilitate integration testing


  // list of all service names
  override def services(): Seq[String] = {
    client.children(svcKey)
  }

  // list of all environment names
  override def environments(): Seq[String] = {
    client.children(envKey)
  }

  // return info about an instance
  override def instance(service: String, environment: String): JsonObject = {
    if (! client.has(s"$svcKey/$service/$envKey/$environment")) {
      throw BadInputException(s"instance: $service:$environment does not exist")
    }
    client.get(s"$svcKey/$service/$envKey/$environment")
  }

  // list of all instances for a service
  override def instances(service: String): Seq[String] = {
    if (! client.has(s"$svcKey/$service")) {
      throw BadInputException(s"service: $service does not exist")
    }
    client.children(s"$svcKey/$service/$envKey")
  }

  // return info about a server
  override def server(service: String, environment: String, subInstance:String, server: String): JsonObject = {
    if (! client.has(s"$svcKey/$service/$envKey/$environment/$subKey/$subInstance/$serverKey/$server")) {
      throw BadInputException(s"server: $service:$environment:$server does not exist")
    }
    client.get(s"$svcKey/$service/$envKey/$environment/$subKey/$subInstance/$serverKey/$server")
  }

  // return info about a service
  override def service(service: String): JsonObject = {
    if (! client.has(s"$svcKey/$service")) {
      throw BadInputException(s"service: $service does not exist")
    }
    client.get(s"$svcKey/$service")
  }

  // return info about an environment
  override def environment(environment: String): JsonObject = {
    if (! client.has(s"$envKey/$environment")) {
      throw new BadInputException(s"environment: $environment does not exist")
    }
    client.get(s"$envKey/$environment")
  }

  // list of all servers for a service:environment:sub
  override def servers(service: String, environment: String, subInstance:String): Seq[String] = {
    if (! client.has(s"$svcKey/$service/$envKey/$environment/$subKey/$subInstance")) {
      throw BadInputException(s"instance: $service:$environment does not exist")
    }
    client.children(s"$svcKey/$service/$envKey/$environment/$subKey/$subInstance/$serverKey")
  }

  // define a new service
  override def defineService(service: String, info: JsonObject): Unit = {
    if (client.has(s"$svcKey/$service")) {
      throw BadInputException(s"service: $service already exists")
    }
    client.put(s"$svcKey/$service", info)
  }

  // define a new environment
  override def defineEnvironment(environment: String, info: JsonObject): Unit = {
    if (client.has(s"$envKey/$environment")) {
      throw BadInputException(s"environment: $environment already exists")
    }
    client.put(s"$envKey/$environment", info)
  }

  // define a new service instance
  override def defineInstance(service: String, environment: String, info: JsonObject): Unit = {
    if (client.has(s"$svcKey/$service/$envKey/$environment")) {
      throw BadInputException(s"service instance: $service:$environment already exists")
    }
    client.put(s"$svcKey/$service/$envKey/$environment", info)
  }

  // define a new a server
  override def defineServer(service: String, environment: String, sub:String, server: String, info: JsonObject): Unit = {
    val path = s"$svcKey/$service/$envKey/$environment/$subKey/$sub/$serverKey/$server"
    if (client.has(path)) {
      throw BadInputException(s"server: $service:$environment:$sub:$server already exists")
    }
    client.put(path, info)
  }

  // list all subs
  override def subs(service:String, environment:String): Seq[String] = {
    if (! client.has(s"$svcKey/$service/$envKey/$environment")) {
      throw BadInputException(s"instance: $service:$environment does not exist")
    }
    client.children(s"$svcKey/$service/$envKey/$environment/$subKey")
  }

  // return info about a sub
  override def sub(service: String, environment: String, sub: String): JsonObject = {
    if (! client.has(s"$svcKey/$service/$envKey/$environment/$subKey/$sub")) {
      throw BadInputException(s"sub: $service:$environment:$sub does not exist")
    }
    client.get(s"$svcKey/$service/$envKey/$environment/$subKey/$sub")
  }

  // define a new sub
  override def updateSub(service: String, environment: String, sub: String, info: JsonObject): Unit = {
    val path = s"$svcKey/$service/$envKey/$environment/$subKey/$sub"
    client.put(path, info)
  }

  // define a new sub
  override def defineSub(service: String, environment: String, sub: String, info: JsonObject): Unit = {
    val path = s"$svcKey/$service/$envKey/$environment/$subKey/$sub"
    if (client.has(path)) {
      throw BadInputException(s"sub: $service:$environment:$sub already exists")
    }
    updateSub(service, environment, sub, info)
  }
}

