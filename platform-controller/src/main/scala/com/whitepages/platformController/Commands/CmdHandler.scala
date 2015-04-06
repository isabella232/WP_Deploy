package com.whitepages.platformController.commands

import java.nio.file.{Files, Paths}

import akka.actor.ActorRefFactory
import com.persist.JsonOps._
import com.whitepages.framework.exceptions.BadInputException
import com.whitepages.framework.logging.{noId, RequestId}
import com.whitepages.framework.util.ClassSupport
import com.whitepages.platformCommon.data.ServiceData
import com.whitepages.platformController.Commands.ShellCommands
import com.whitepages.platformController.client.AgentRequests._
import com.whitepages.platformController.client.DockerClient
import com.whitepages.platformController.coordinator.Coordinator
import com.whitepages.platformController.locks.LockManager
import com.whitepages.platformController.provision.Provisioner
import com.whitepages.platformUtil.Order
import scala.concurrent.duration._
import scala.language.postfixOps

// TODO lots of work getting lock all set right
// TODO note multiple locks will be required

case class CmdHandler(factory: ActorRefFactory, data: ServiceData, repo: DockerClient) extends ClassSupport with ShellCommands {

  val lm = LockManager()

  val provisioner = Provisioner(factory)

  private def servers(service: String, environment: String, sub: String, server: String): Seq[String] = {
    if (server == "") {
      val r = data.servers(service, environment, sub)
      r
    } else {
      // TODO check that servers are valid
      if (server.contains(",")) {
        val s1 = server.replaceAll("[", "").replace("]", "").replace(" ", "")
        s1.split(",").toSeq
      } else {
        Seq(server)
      }
    }
  }

  def doCmd(id: RequestId, user: String, in: JsonObject,
            progress: (JsonObject) => Unit,
            query: Option[(String, Seq[String]) => String]): Json = {

    def progressHandler(p: Progress): Unit = {
      val info = JsonObject("cmd" -> "progress", "host" -> p.host, "msg" -> p.msg, "level" -> p.level)
      progress(info)
    }

    def getInfo(name: String, vals: Seq[String], checkVal: Boolean = true): String = {
      val v1 = jgetString(in, "info", name)
      val v2 = if (v1 != "") {
        v1
      } else {
        val result = query match {
          case Some(q) => q(name, vals)
          case None => throw BadInputException(s"$name not specified")
        }
        if (result == "**cancel**") throw BadInputException("canceled")
        result
      }
      if (checkVal && !vals.contains(v2)) {
        throw BadInputException(s"$name bad value:$v2")
      }
      v2
    }

    def getIntegerInfo(name: String): Long = {
      val v0 = getInfo(name, Seq("Integer value", s"for $name"), checkVal = false)
      try {
        v0.toLong
      } catch {
        case ex: NumberFormatException => throw new BadInputException(s"$name is not an integer: $v0")
        case other: Throwable =>
          log.error(noId, "Caught unexpected exception", other)
          throw new BadInputException(s"Unexpected exception ${other.getMessage}")
      }
    }

    val cmd = jgetString(in, "cmd")
    val service = jgetString(in, "svc")
    val environment = jgetString(in, "env")
    val sub = jgetString(in, "sub")
    val server = jgetString(in, "svr")
    val group = jgetString(in, "grp")
    val version = jgetString(in, "ver")
    val password = jgetString(in, "password")
    // TODO remove password from log
    log.alternative("cmd", in)

    def protect: Unit = {
      // don't let most users modify platform controller
      // TODO check isLocal (from crypto)
      if (service == "platform-controller" && password != "foo") {
        throw BadInputException("command not authorized")
      }
    }

    cmd match {
      case "svc" =>
        // return list of services
        data.services()
      case "env" =>
        if (service == "") {
          data.environments()
        } else {
          data.instances(service)
        }
      case "sub" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        data.subs(service, environment)
      case "svr" =>
        // return list of servers for a service:environment:sub
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        servers(service, environment, sub, server)
      case "" =>
        if (service == "" && environment != "") {
          data.environment(environment)
        } else {
          if (service == "") throw BadInputException("service not specified")
          if (environment == "") {
            val v = data.service(service)
            if (v == emptyJsonObject) throw BadInputException(s"service: $service does not exist")
            v
          } else if (sub == "") {
            data.instance(service, environment)
          } else if (server == "") {
            data.sub(service, environment, sub)
          } else {
            data.server(service, environment, sub, server)
          }
        }
      case "ver" =>
        if (service == "") throw BadInputException("service not specified")
        val group = jgetString(data.service(service), "group")
        val j = repo.call(s"/v1/repositories/$group/$service/tags")
        val v = jgetObject(j) map {
          case (k, v) => k
        }

        val v1 = v.toSeq map {
          case v: String => (Order.sortKey(v), v)
        }
        val v2 = v1.sortWith {
          case ((asort, a), (bsort, b)) =>
            asort > bsort
        }
        val versions = v2 map {
          case (asort, a) => a
        }
        versions
      case "status" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val group = jgetString(data.service(service), "group")
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val localHost = ""
        val coord = Coordinator(id, factory, service, group, environment, sub, localHost, serverNames, user, Some(progressHandler))
        val result = coord.fromAll("status")
        coord.close
        result
      case "info" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val group = jgetString(data.service(service), "group")
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val localHost = ""
        val coord = Coordinator(id, factory, service, group, environment, sub, localHost, serverNames, user, Some(progressHandler))
        val result = coord.fromAll("info")
        coord.close
        result
      case "downloads" =>
        // lock
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val group = jgetString(data.service(service), "group")
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val localHost = ""
        val coord = Coordinator(id, factory, service, group, environment, sub, localHost, serverNames, user, Some(progressHandler))
        val info = coord.fromAll("downloads")
        coord.close
        info
      case "download" =>
        // lock
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        if (version == "") throw BadInputException("version not specified")
        val group = jgetString(data.service(service), "group")
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val localHost = ""
        val coord = Coordinator(id, factory, service, group, environment, localHost, sub, serverNames, user, Some(progressHandler))
        coord.download(service, version)
        coord.close
        emptyJsonObject
      case "deploy" =>
        protect
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        if (version == "") throw BadInputException("version not specified")
        val serviceInfo = data.service(service)
        val group = jgetString(serviceInfo, "group")
        val select = jgetArray(serviceInfo, "select").map {
          jgetString(_)
        }
        val instanceInfo = data.instance(service, environment)
        val hasLB = jgetString(instanceInfo, "lb") != "none"
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val localHost = ""
        val info = data.sub(service, environment, sub)
        val info1 = jgetObject(jput(info, "version")(version))
        lm.lock(s"SVC:$service:ENV:$environment", 5 minutes) {
          data.updateSub(service, environment, sub, info1)
          val coord = Coordinator(id, factory, service, group, environment, sub, localHost, serverNames, user, Some(progressHandler))
          val domain = jgetString(info1, "domain")
          coord.deploy(service, version, select, hasLB, domain)
          coord.close
          emptyJsonObject
        }
      case "halt" =>
        protect
        // lock
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val group = jgetString(data.service(service), "group")
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val info = data.sub(service, environment, sub)
        val info1 = jgetObject(jput(info, "version")(""))
        data.updateSub(service, environment, sub, info1)
        val localHost = ""
        val coord = Coordinator(id, factory, service, group, environment, sub, localHost, serverNames, user, Some(progressHandler))
        coord.halt()
        coord.close
        emptyJsonObject
      case "bounce" =>
        protect
        //lock
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val group = jgetString(data.service(service), "group")
        val serverNames = servers(service, environment, sub, server)
        // TODO used only for "*" host
        val localHost = ""
        val coord = Coordinator(id, factory, service, group, environment, sub, localHost, serverNames, user, Some(progressHandler))
        coord.bounce()
        coord.close
        emptyJsonObject
      case "addSvc" =>
        // TODO letter { letter | digit }*
        if (service == "") throw BadInputException("service not specified")
        val group = getInfo("group", Seq("platform", "search", "mobile", "data"))
        val port = getIntegerInfo("port")
        lm.lock(s"SVC:$service", 20 seconds) {
          val obj = JsonObject("group" -> group, "port" -> port)
          data.defineService(service, obj)
          obj
        }
      case "addEnv" =>
        // TODO letter { letter | digit }*
        // TODO make sure env does not already exist
        if (environment == "") throw BadInputException("environment not specified")
        val zone = getInfo("zone", Seq("util", "qa", "staging", "prod"))
        lm.lock(s"ENV:$environment", 20 seconds) {
          val obj = JsonObject("zone" -> zone)
          data.defineEnvironment(environment, obj)
          obj
        }
      case "addIns" =>
        // TODO make sure instance does not already exist
        // TODO make sure service and env exist
        protect
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        // TODO lock ???
        val lb = getInfo("loadBalancer", Seq("none", "whitepages"))
        val select = getInfo("select", Seq.empty[String], checkVal = false)
        val s1 = select.trim()
        if (!s1.startsWith("[") || !s1.endsWith("]")) throw BadInputException("Missing [] in select")
        val s2 = s1.substring(1, s1.size - 1)
        val s3 =
          if (s2.isEmpty) Seq.empty[String]
          else            s2.split(",").map(_.trim()).toSeq
        // TODO letter { letter | digit }*
        val obj = JsonObject("lb" -> lb, "select" -> s3)
        data.defineInstance(service, environment, obj)
        provisioner.addInstance()
        obj
      case "addSub" =>
        // TODO letter { letter | digit }*
        // TODO make sure set does not already exist
        // TODO progress???
        protect
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        // TODO allow other subs
        val count = getIntegerInfo("count").toInt
        val provider = getInfo("provider", Seq("aws", "openstack"))
        val instanceType = if (provider == "aws") {
          getInfo("instanceType", Seq(
            "t1.micro", "t2.micro", "t2.small", "t2.medium",
            "m1.small", "m1.medium", "m1.large", "m1.xlarge",
            "m2.xlarge", "m2.2xlarge", "m2.4xlarge",
            "m3.medium", "m3.large", "m3.xlarge", "m3.2xlarge",
            "c1.medium", "c1.xlarge",
            "c3.large", "c3.xlarge", "c3.2xlarge", "c3.4xlarge", "c3.8xlarge",
            "c4.large", "c4.xlarge", "c4.2xlarge", "c4.4xlarge", "c4.8xlarge",
            "r3.large", "r3.xlarge", "r3.2xlarge", "r3.4xlarge", "r3.8xlarge",
            "i2.xlarge", "i2.2xlarge", "i2.4xlarge", "i2.8xlarge",
            "cc2.8xlarge", "g2.2xlarge", "cr1.8xlarge", "hi1.4xlarge", "hs1.8xlarge"
          ))
        } else {
          getInfo("instanceType", Seq("m1.small", "m1.medium", "m1.large", "m1.xlarge",
            "m3.medium", "m3.large", "m3.large", "m3.2xlarge",
            "c3.large", "c3.xlarge", "c3.2xlarge", "c3.4xlarge", "c3.5xlarge",
            "wp4.60xlarge"
          ))
        }
        // TODO check weight should be in range 1..100
        val weight = getIntegerInfo("weight").toInt
        val serviceInfo = data.service(service)
        val port = jgetInt(serviceInfo, "port")
        val environmentInfo = data.environment(environment)
        val zone = jgetString(environmentInfo, "zone")
        val obj = JsonObject("count" -> count, "provider" -> provider,
          "instanceType" -> instanceType, "weight" -> weight,
          "version" -> "")
        val instanceInfo = data.instance(service, environment)
        val lb = jgetString(instanceInfo, "lb")
        val select = jgetArray(instanceInfo, "select") map (jgetString(_))
        lm.lock(s"SVC:$service:ENV:$environment", 5 minutes) {
          data.defineSub(service, environment, sub, obj)
          if (count == 0) {
            // For testing only
            data.defineServer(service, environment, sub, "0.0.0.1", emptyJsonObject)
          } else {
            if (sub != "main") throw BadInputException("sub must be main (for now)")
            for (ip <- provisioner.addSub(service, environment, sub, zone,
              port, provider, instanceType, count, weight, lb, select)) {
              data.defineServer(service, environment, sub, ip, emptyJsonObject)
            }
          }
          obj
        }
      case "upgradeSub" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        // TODO do a rolling reprovision to update coreos and standard containers
        provisioner.upgradeSub()
        throw BadInputException("upgradeSub NYI")
      case "changeWeight" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val weight = getIntegerInfo("weight")
        val info = data.sub(service, environment, sub)
        val info1 = jgetObject(jput(info, "weight")(weight))
        data.updateSub(service, environment, sub, info1)
        provisioner.changeWeight()
        throw BadInputException("changeWeight NYI")
      case "changeCount" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        val count = getIntegerInfo("count").toInt
        val info = data.sub(service, environment, sub)
        val info1 = jgetObject(jput(info, "count")(count))
        data.updateSub(service, environment, sub, info1)
        provisioner.changeCount()
        throw BadInputException("changeSize NYI")
      case "deleteSub" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        if (sub == "") throw BadInputException("sub not specified")
        provisioner.deleteSub()
        // TODO make sure all servers are halted
        throw BadInputException("delete NYI")
      case "deleteIns" =>
        if (service == "") throw BadInputException("service not specified")
        if (environment == "") throw BadInputException("environment not specified")
        // TODO make sure there are no subs
        provisioner.deleteInstance()
        throw BadInputException("deleteIns NYI")
      case "deleteSvc" =>
        if (service == "") throw BadInputException("service not specified")
        // TODO make sure there are no instances
        throw BadInputException("deleteSvc NYI")
      case "deleteEnv" =>
        if (environment == "") throw BadInputException("environment not specified")
        // TODO make sure there are no instances
        throw BadInputException("deleteEnv NYI")
      case "forceUpdate" =>
        // TODO authenticate -- check password
        val container = jgetString(in, "cont")
        if (server == "") throw BadInputException("server not specified")
        if (container != "agent") throw BadInputException("container agent not specified")
        if (version == "") throw BadInputException("version not specified")
        val nameMap = Map("agent" -> "service-agent")
        val home = Option(System.getenv("HOME")) match {
          case Some(homeDir) if Files.isRegularFile(Paths.get(homeDir, ".ssh", "platform_id_rsa.private")) =>
            doUpdate(homeDir, container, server, version, nameMap)
          case _ => new BadInputException("HOME environment variable not set/missing ~/.ssh/platform_id_rsa.private key")
        }
        emptyJsonObject
      case x: String =>
        throw BadInputException(s"Unrecognized command: $x")
    }
  }

}
