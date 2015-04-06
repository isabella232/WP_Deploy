package com.whitepages.serviceDeploy.cmdHandler

import com.persist.Exceptions.SystemException
import com.whitepages.platformControllerClient.ControllerClient
import scala.io.Source
import com.persist.JsonOps._
import sys.process._
import scala.sys.process.ProcessLogger

case class ServicesInfo(localHost: String, local: Boolean, pc: Boolean, controllerClient: ControllerClientWrapper) {

  private[this] val (where, text) =
    if (pc) {
      ("", "")
    } else if (local) {
      val fname = "config/servers.json"
      try {
        (s"local file: $fname", Source.fromFile(fname).getLines().mkString("\n"))
      } catch {
        case ex: Throwable => throw new Exception(s"Can't read local file: $fname")
      }
    } else {
      def ignore1(s: String) {}
      def ignore2(s: String) {}
      val pl = ProcessLogger(ignore1, ignore2)
      val url = "https://github.dev.pages/search-dev/deploy-config/raw/master/servers.json"
      try {
        (s"GITHUB: $url", Seq[String]("curl", "-f", url) !! (pl))
      } catch {
        case ex: Throwable =>
          throw new Exception(s"Failure when fetching GITHUB: $url")
      }
    }


  val j = if (pc) {
    emptyJsonObject
  } else {
    try {
      Json(text)
    } catch {
      case ex: SystemException =>
        println(s"${Console.RED}Bad JSON from $where${Console.RESET}")
        println(Pretty(ex.info))
        throw new Exception("Bad Json")
    }
  }
  private[this] val data = if (pc) {
    emptyJsonObject
  } else {
    jgetObject(Json(text))

  }

  //println(local + ":" + Pretty(data))

  private[this] val services = jgetArray(data, "services")

  val lookup = jgetString(data, "lookup")


  private def progress(p: ControllerClient.Progress) {}

  private def query(name: String, vals: Seq[String]): String = {
    ""
  }

  def getServices(): Seq[String] = {
    if (pc) {
      val r = controllerClient.callSeq("svc", emptyJsonObject, progress, query)
      //println("pc=" + Pretty(r))
      r.map {
        jgetString(_)
      }
    } else {
      services map {
        case j => jgetString(j, "service")
      }
    }
  }

  def getEnvironments(): Seq[String] = {
    if (pc) {
      val r = controllerClient.callSeq("env", emptyJsonObject, progress, query)
      r.map {
        jgetString(_)
      }
    } else {
      services map {
        case j => jgetString(j, "service")
      }
    }
  }

  def getInstances(service: String): Seq[String] = {
    val r = controllerClient.callSeq("env", JsonObject("svc" -> service), progress, query)
    r.map {
      jgetString(_)
    }
  }

  def getSubs(service: String, environment: String): Seq[String] = {
    val r = controllerClient.callSeq("sub", JsonObject("svc" -> service, "env" -> environment), progress, query)
    r.map {
      jgetString(_)
    }

  }

  def getVersions(service: String): Seq[String] = {
    val options = JsonObject("svc" -> service)
    val r = controllerClient.callSeq("ver", options, progress, query)
    //println("pc=" + Pretty(r))
    r.map {
      jgetString(_)
    }
  }

  def getService(service: String): Option[Json] = {
    if (pc) {
      val options = JsonObject("svc" -> service)
      val r = controllerClient.callCond("", options, progress, query)
      r
    } else {
      (services filter (jgetString(_, "service") == service)).headOption
    }
  }

  def getEnvironment(environment: String): Option[Json] = {
    val options = JsonObject("env" -> environment)
    val r = controllerClient.callCond("", options, progress, query)
    r
  }

  def getServiceProperties(service: String): Json = {
    emptyJsonObject
  }

  def getEnvs(service: String): Option[Seq[String]] = {
    if (pc) {
      val options = JsonObject("svc" -> service)
      val r = controllerClient.callSeq("env", options, progress, query)
      //println("pc=" + Pretty(r))
      Some(r.map {
        jgetString(_)
      })
    } else {
      getService(service) match {
        case Some(sss) =>
          Some(jgetArray(sss, "envs") map {
            case j => jgetString(j, "env")
          })
        case None => None
      }

    }
  }

  def getEnv(service: String, env: String): Option[Json] = {
    if (pc) {
      val options = JsonObject("svc" -> service, "env" -> env)
      val r = controllerClient.callObj("", options, progress, query)
      // TODO no such instance
      Some(r)

    } else {
      getService(service) match {
        case Some(s) =>
          (jgetArray(s, "envs") filter (jgetString(_, "env") == env)).headOption
        case None => None
      }
    }
  }

  def getSub(service: String, env: String, sub: String): Option[Json] = {
    val options = JsonObject("svc" -> service, "env" -> env, "sub" -> sub)
    val r = controllerClient.callObj("", options, progress, query)
    // TODO no such sub
    Some(r)
  }

  def fixServerName(j: Json): String = {
    val n = jgetString(j, "server")
    //if (n == "*") "localHost" else n
    n
  }

  def getProvisioner(service: String, env: String): Option[String] = {
    if (pc) {
      val options = JsonObject("svc" -> service, "env" -> env)
      val r = controllerClient.callSeq("svr", options, progress, query)
      Some(r.map {
        jgetString(_)
      }.head)
    } else {
      getEnv(service, env) match {
        case None => None
        case Some(sss) =>
          jgetString(sss, "provisioner") match {
            case "*" => Some(localHost)
            case "" => getServers(service, env).getOrElse(Nil).headOption
            case n => Some(n)
          }
      }
    }
  }

  def getServers(service: String, env: String): Option[Seq[String]] = {
    if (pc) {
      val options = JsonObject("svc" -> service, "env" -> env, "sub" -> "main")
      val r = controllerClient.callSeq("svr", options, progress, query)
      //println("pc=" + Pretty(r))
      Some(r.map {
        jgetString(_)
      })

    } else {
      getEnv(service, env) match {
        case Some(sss) =>
          Some(jgetArray(sss, "servers") map {
            case j =>
              val n = fixServerName(j)
              if (n == "*") localHost else n
          })
        case None => None
      }
    }
  }

  def getServer(service: String, env: String, server: String): Option[Json] = {
    if (pc) {
      val options = JsonObject("svc" -> service, "env" -> env, "svr" -> server)
      val r = controllerClient.callObj("", options, progress, query)
      //println("pc=" + Pretty(r))
      // TODO not such service
      //Some(jgetObject(r))
      Some(r)
    } else {
      getEnv(service, env) match {
        case Some(s) =>
          (jgetArray(s, "servers") filter (fixServerName(_) == server)).headOption
        case None => None
      }
    }
  }

  // ************************************* below to be removed!!!!
  def getLoadBalancerPort(service: String, env: String): Option[Int] = {
    getEnv(service, env) match {
      case None => None
      case Some(s) => Some(jgetInt(s, "lb", "port"))
    }
  }

  def getDomain(service: String, env: String): Option[String] = {
    getEnv(service, env) match {
      case None => None
      case Some(s) => Some(jgetString(s, "domain"))
    }
  }

  def getDefaultInstance(service: String, env: String): Option[String] = {
    getEnv(service, env) match {
      case None => None
      case Some(s) =>
        jgetString(s, "defaultInstance") match {
          case "" => None
          case m => Some(m)
        }
    }
  }

  def getProviderRegionZone(service: String, env: String): (Option[String], Option[String], Option[String]) = {
    getEnv(service, env) match {
      case None => (None, None, None)
      case Some(s) =>
        (jgetString(s, "defaultProvider") match {
          case "" => None
          case m => Some(m)
        },
          jgetString(s, "region") match {
            case "" => None
            case m => Some(m)
          },
          jgetString(s, "zone") match {
            case "" => None
            case m => Some(m)
          })
    }
  }

  def getMinMaxServers(service: String, env: String): (Option[Int], Option[Int]) = {
    getEnv(service, env) match {
      case None => (None, None)
      case Some(s) => (Some(jgetInt(s, "minServers")), Some(jgetInt(s, "maxServers")))
    }
  }

}
