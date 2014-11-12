package com.whitepages.serviceDeploy.cmdHandler

import com.persist.Exceptions.SystemException

import scala.io.Source
import com.persist.JsonOps._
import sys.process._
import scala.sys.process.ProcessLogger

case class ServicesInfo(localHost:String, local:Boolean) {

  private[this] val (where, text) = if (local) {
    val fname = "config/servers.json"
    try {
      (s"local file: $fname", Source.fromFile(fname).getLines().mkString("\n"))
    } catch {
      case ex:Throwable => throw new Exception(s"Can't read local file: $fname")
    }
  } else {
    def ignore1(s: String) {}
    def ignore2(s: String) {}
    val pl = ProcessLogger(ignore1, ignore2)
    //val url = "https://github.dev.pages/search-dev/service-deploy/raw/master/config/servers.json"
    val url = "https://github.dev.pages/search-dev/deploy-config/raw/master/servers.json"
    try {
      (s"GITHUB: $url", Seq[String]("curl", "-f", url) !! (pl))
    } catch {
      case ex:Throwable =>
        throw new Exception(s"Failure when fetching GITHUB: $url")
    }
  }


  val j = try {
    Json(text)
  } catch {
    case ex:SystemException =>
      println(s"${Console.RED}Bad JSON from $where${Console.BLACK}")
      println(Pretty(ex.info))
      throw new Exception("Bad Json")
  }
  private[this] val data = jgetObject(Json(text))

  //println(local + ":" + Pretty(data))

  private[this] val services = jgetArray(data, "services")

  val lookup = jgetString(data, "lookup")


  def getServices(): Seq[String] = {
    services map {
      case j => jgetString(j, "service")
    }
  }

  def getService(service: String): Option[Json] = {
    (services filter (jgetString(_, "service") == service)).headOption
  }

  def getStacks(service: String): Option[Seq[String]] = {
    getService(service) match {
      case Some(sss) =>
        Some(jgetArray(sss, "stacks") map {
          case j => jgetString(j, "stack")
        })
      case None => None
    }
  }

  def getStack(service: String, stack: String): Option[Json] = {
    getService(service) match {
      case Some(s) =>
        (jgetArray(s, "stacks") filter (jgetString(_, "stack") == stack)).headOption
      case None => None
    }
  }

  def fixServerName(j: Json): String = {
    val n = jgetString(j, "server")
    //if (n == "*") "localHost" else n
    n
  }

  def getServers(service: String, stack: String): Option[Seq[String]] = {
    getStack(service, stack) match {
      case Some(sss) =>
        Some(jgetArray(sss, "servers") map {
          case j =>
            val n = fixServerName(j)
            if (n == "*") localHost else n
        })
      case None => None
    }
  }

  def getServer(service: String, stack: String, server: String): Option[Json] = {
    getStack(service, stack) match {
      case Some(s) =>
        (jgetArray(s, "servers") filter (fixServerName(_) == server)).headOption
      case None => None
    }
  }

}
