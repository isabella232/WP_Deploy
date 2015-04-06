package com.whitepages.platformCommon.data

import com.persist.JsonOps._

trait ServiceData {

  // list of all service names
  def services(): Seq[String]

  // list of all environment names
  def environments(): Seq[String]

  // list of all environments for a service
  def instances(service: String): Seq[String]

  // list of all servers for a service:environment
  def servers(service: String, environment: String, sub:String): Seq[String]

  // return info about a service
  def service(service: String): JsonObject

  // define a new service
  def defineService(service: String, info: JsonObject)

  // return info about an environment
  def environment(environment: String): JsonObject

  // define a new environment
  def defineEnvironment(environment: String, info: JsonObject)

  // define a new service instance
  def defineInstance(service: String, environment: String, info: JsonObject)

  // return info about an instance
  def instance(service: String, environment: String): JsonObject

  // list all subs
  def subs(service:String, environment:String): Seq[String]

  // update an existing sub
  def updateSub(service: String, environment: String, sub: String, info: JsonObject)

  // define a new sub
  def defineSub(service: String, environment: String, sub: String, info: JsonObject)

  // return info about a sub
  def sub(service: String, environment: String, sub: String): JsonObject

  // return info about a server
  def server(service: String, environment: String, sub:String, server: String): JsonObject

  // define a new a server
  def defineServer(service: String, environment: String, sub:String, server: String, info: JsonObject)
}
