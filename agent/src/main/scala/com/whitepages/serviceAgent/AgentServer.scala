package com.whitepages.serviceAgent

import java.util.concurrent.TimeoutException

import akka.actor.{ActorRef, Props}
import com.persist.Exceptions.SystemException
import com.persist.JsonOps._
import com.typesafe.config.ConfigFactory
import com.whitepages.framework.exceptions._
import com.whitepages.framework.logging.noId
import com.whitepages.framework.service.JsonService
import com.whitepages.framework.util.ActorSupport
import com.whitepages.info.service_agent.BuildInfo
import com.whitepages.platformCommon.crypto.Credentials
import com.whitepages.platformCommon.jclouds.InstanceRequest
import com.whitepages.platformCommon.orchestration.Orchestrator
import com.whitepages.platformCommon.orchestration.Orchestrator.BuildService
import com.whitepages.serviceAgent.Requests._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.language.postfixOps


object AgentServer {
  val ipEnvironmentVariable = "FRAMEWORK_IP"

  def getProps(agentHost: String, svcHost: String, service: JsonService) = Props(classOf[AgentServer], agentHost, svcHost, service)
}


class AgentServer(agentHost: String, svcHost: String, service: JsonService) extends ActorSupport {

  case object Free

  case object ShutDown

  private[this] val registryName = config.getString("wp.service-agent.registry")
  private[this] val useRegistry = config.getBoolean("wp.service-agent.useRegistry")
  private[this] implicit val ec: ExecutionContext = context.dispatcher
  private[this] val docker = DockerClient("docker", this.context)
  private[this] val repo = if (useRegistry) DockerClient("repo", this.context) else null
  private[this] val serviceIpAddress = System.getenv(AgentServer.ipEnvironmentVariable)
  private[this] val defaultHostname = "default"

  sealed case class Clients(loadBalancerOpt: Option[LoadBalancer], lifecycle: ActorRef)

  private[this] var clientsOpt: Option[Clients] = None


  def createClients(serviceName: String, balancerScope: String, svcHost: String, svcPort: Int): Clients = {
    val loadBalancerOpt =
      if (balancerScope.nonEmpty) {
        val lbConfig =
          if (config.hasPath(s"wp.balancer.$balancerScope")) config.getConfig(s"wp.balancer.$balancerScope")
          else {
            log.warn(noId, s"could not find load balancer configuration for balancer.$balancerScope; falling back to null configuration")
            ConfigFactory.empty("Null load balancer")
          }
        Some(LoadBalancer(serviceName, balancerScope, serviceIpAddress, svcPort, lbConfig))
      } else None
    /*
    The lifecycle actor must be (i) top-level and (ii) named as such as it is looked up by remote actors
     */
    val lifecycle = system.actorOf(LifecycleClient.getProps(svcHost, loadBalancerOpt), "lifecycle")
    val clients = Clients(loadBalancerOpt, lifecycle)
    clientsOpt = Some(clients)
    clients
  }

  def connectSignalHandlers() {
    val sh = new sun.misc.SignalHandler {
      def handle(sig: sun.misc.Signal) {
        log.error(noId, "signal:" + sig.toString)
        //StateFile.set("SHUTDOWN",sig.toString)
        if (sig.toString == "SIGTERM") {
          log.error(noId, "STARTING FORCED SHUTDOWN")
          self ! ShutDown
        }
      }
    }
    // TERM for docker shutdown
    val s = new sun.misc.Signal("TERM")
    sun.misc.Signal.handle(s, sh)
    // INT for testing ^C
    val s1 = new sun.misc.Signal("INT")
    //sun.misc.Signal.handle(s1, sh)
    log.info(noId, "Signal handlers ready")
  }

  log.info(noId, JsonObject("state" -> StateFile.read()))
  private[this] val (stateName, state) = StateFile.getFirst()

  if (stateName != "") {
    //log.info(noId, JsonObject("agentStartState" -> state))
    val svcPort = jgetInt(state, "lifecyclePort")
    //val hasLB = jgetBoolean(state, "hasLB")
    val hasLB = jgetBoolean(state, "request", "hasLB")
    val group = jgetString(state, "group")
    val serviceName = jgetString(state, "service")
    val httpPort = jgetInt(state, "httpPort")
    val environment = jgetString(state, "env")
    val balancerScope = jgetString(state, "request", "domain").split('.').head
    val clients = createClients(serviceName, balancerScope, svcHost, httpPort)
    clients.lifecycle ! LcInit(hasLB, svcPort)
    //val version = jgetString(state, "version")
    val version = jgetString(state, "request", "version")
    val running = container(serviceName, group)
    if (running == None) {
      def report(s: String, j: Json) {}
      doCmd("deploy", jgetObject(state), report)
    }
  }

  connectSignalHandlers()

  // TODO stop docker and repo clients

  /*
  val auth = JsonObject(
    "username" -> "nestorpersist",
    "password" -> "quad999",
    "email" -> "nestor@persist.com",
    "serveraddress" -> "https://index.docker.io/v1/"
  )
  val authResponse = docker.call("/auth", body = Some(auth), method = "post")
  log.info(noId, JsonObject("auth" -> authResponse))
  */

  def trimRepo(s: String, serviceName: String, groupName: String): String = {
    val prefix = s"$registryName/$groupName/"
    if (s.startsWith(prefix)) {
      val s1 = s.replaceFirst(prefix, "")
      if (s1.startsWith(serviceName)) s1 else s
    } else {
      s
    }
  }

  private def getVersionStatus(j: JsonObject): JsonObject = {
    JsonObject("version" -> jget(j, "version"), "status" -> jget(j, "status"))
  }

  def reply(cmd: String, r: Json): String = {
    Compact(JsonObject("cmd" -> cmd, "response" -> r))
  }

  def images(serviceName: String, groupName: String): Seq[JsonObject] = {
    val imagesJson = docker.call("/images/json")
    val report0 = jgetArray(imagesJson) flatMap {
      case download =>
        val rtags = jgetArray(download, "RepoTags")
        rtags map {
          case tag =>
            val rtag = jgetString(tag).split(":")
            val size = jgetInt(download, "VirtualSize")
            val created = jgetLong(download, "Created") * 1000
            JsonObject("fullService" -> rtag(0), "version" -> rtag(1), "size" -> size, "created" -> created)
        }
    }
    val report = report0 filter {
      case item =>
        trimRepo(jgetString(item, "fullService"), serviceName, groupName) == serviceName
    }
    val report1 = report map {
      case d => jgetObject(jdelete(d, "fullService"))
    }
    report1
  }

  def image(name: String): Option[JsonObject] = {
    try {
      //log.info(noId, name)
      //log.error(noId, JsonObject("imagepath" -> s"/images/$name/json"))
      val imageJson = docker.call(s"/images/$name/json")
      //log.info(noId, imageJson)
      val config = jget(imageJson, "Config")
      val ports = jgetObject(config, "ExposedPorts") map {
        case (n, value) =>
          n.split("/")(0).toInt
      }
      Some(JsonObject("ports" -> ports.toSeq))
    } catch {
      case ex: Throwable => None // can't find image
    }
  }

  def container(serviceName: String, groupName: String): Option[JsonObject] = {
    val r1 = docker.call("/containers/json")
    val containersJson = jgetArray(r1).filter {
      case containerJson =>
        val parts = trimRepo(jgetString(containerJson, "Image"), serviceName, groupName).split(":")
        parts(0) == serviceName
    }
    if (containersJson.size == 0) {
      None
    } else {
      val containerJson = jget(containersJson, 0)
      val image = jgetString(containerJson, "Image")
      val stat = jgetString(containerJson, "Status")
      val id = jgetString(containerJson, "Id")
      val parts = trimRepo(jgetString(containerJson, "Image"), serviceName, groupName).split(":")
      println(Compact(parts.toSeq))
      val j = Some(JsonObject("image" -> image, "version" -> parts(1),
        "status" -> stat, "id" -> id))
      j
    }
  }

  def doCmd(requestCmd: String, request: JsonObject,
            report: (String, JsonObject) => Unit): JsonObject = {

    def reportProgress(p: Progress) {
      report("progress", JsonObject("msg" -> p.msg, "level" -> p.level) ++ p.extra)
    }

    def fail(msg: String) = {
      report("fail", JsonObject("msg" -> msg))
    }

    def progress(msg: String, level: String = "info") {
      reportProgress(Progress(msg, level = level))
    }

    def delay {
      report("delay", emptyJsonObject)
    }

    def run(request: JsonObject): JsonObject = {
      log.error(noId, JsonObject("run" -> request))
      val serviceName = jgetString(request, "service")
      val groupName = jgetString(request, "group")
      val version = jgetString(request, "request", "version")
      val hasLB = jgetBoolean(request, "request", "hasLB")
      val environment = jgetString(request, "request", "envName")
      val select = jgetArray(request, "request", "select") map (jgetString(_))
      //val version = jgetString(request, "version")
      //val hasLB = jgetBoolean(request, "hasLB")
      //val select = jgetArray(request, "select") map (jgetString(_))
      val imageName = s"$registryName/$groupName/$serviceName:$version"
      //log.error(noId, JsonObject("runimage" -> imageName))

      image(imageName) match {
        case None =>
          progress("can't find image")
          emptyJsonObject
        case Some(imageInfo) =>
          //status() match {
          //case Some(container) =>
          // TODO if running stop it
          //throw NotAvailableException("container currently running")
          //case None =>
          progress(s"creating container for $version")
          //val agentHost = config.getString("akka.remote.netty.tcp.hostname")
          //val svcHost = "192.168.59.103"

          val ports = jgetArray(imageInfo, "ports")
          val lifecyclePort = ports.find {
            case port => jgetInt(port) / 10000 == 3
          } match {
            case Some(hd) => jgetInt(hd)
            case None =>
              log.error(noId, s"Could not determine lifecycle port from '${Compact(ports)}'")
              39999 // TODO this is an error
          }

          val httpPort = ports.find {
            case port => jgetInt(port) / 10000 == 0
          } match {
            case Some(hd) => jgetInt(hd)
            case None =>
              log.error(noId, s"Could not determine service HTTP port from '${Compact(ports)}'")
              -1 // TODO this is an error
          }

          val hostname = Option(System.getenv("FQDN")) match {
            case Some(fqdn) => fqdn
            case None => defaultHostname
          }

          /*
          Pull private key injected by platform-controller, if any
           */
          val injectedKey =
            if (serviceName == "platform-controller") JsonArray(s"WP_SECRET=${Credentials.getPrivate()}")
            else emptyJsonArray

          val env = Compact(JsonObject("agentHost" -> agentHost
            , "containerHost" -> svcHost
            , "lifecyclePort" -> lifecyclePort
            , "configSelect" -> select
          ))

          val responsej = docker.call("/containers/create", body = Some(
            JsonObject("Image"    -> imageName,
                       "Env"      -> (JsonArray(s"serviceInfo=$env") ++ injectedKey),
                       "Hostname" -> hostname)))
          val id = jgetString(responsej, "Id")

          StateFile.set(serviceName, jgetObject(request) ++
            JsonObject("httpPort" -> httpPort
              , "svcHost" -> svcHost
              , "lifecyclePort" -> lifecyclePort
              , "agentHost" -> agentHost)
            , reportProgress = Some(reportProgress)
          )
          val p = Promise[LcResult]()
          if (clientsOpt == None) {
            val balancerScope = jgetString(request, "request", "domain").split('.').head
            createClients(serviceName, balancerScope, svcHost, httpPort)
          }
          clientsOpt foreach { clients =>
            clients.lifecycle ! LcInit(hasLB, lifecyclePort)
            clients.lifecycle ! LcRun(p, reportProgress, imageName, select)
          }

          progress("starting container")
          val portSpecs = ports map {
            case port => (s"$port/tcp", JsonArray(JsonObject("HostIp" -> "0.0.0.0", "HostPort" -> port.toString)))
          }
          // Use NetworkMode = host until akka can listen on 0.0.0.0
          val drequest = JsonObject("PortBindings" -> portSpecs.toMap, "NetworkMode" -> "host")
          val responset = docker.call(s"/containers/$id/start", body = Some(drequest))

          Await.result(p.future, Duration.Inf)
          emptyJsonObject
        //}
      }
    }

    def halt(serviceName: String, groupName: String, clear: Boolean = true): JsonObject = {
      container(serviceName, groupName) match {
        case None =>
          progress("no container currently running")
          emptyJsonObject
        case Some(containerJson) =>
          //log.warn(noId, containerJson)
          val (svcName, info) = StateFile.getFirst(Some(reportProgress))
          if (svcName != "" && clear) {
            StateFile.clear(svcName, Some(reportProgress))
          } else {
            //val (serviceName, info) = StateFile.getFirst()
            //val info1 = jput(jgetObject(info),"HALT")("******")
            //StateFile.set(serviceName, info1)
            //log.error(noId, "******** SHUTDOWN")
          }
          val id = jgetString(containerJson, "id")
          //val cname = jgetString(container, "name")
          val image = jgetString(containerJson, "image")

          val p = Promise[LcResult]()
          clientsOpt foreach (clients => clients.lifecycle ! LcStop(p, reportProgress, image))
          Await.result(p.future, 5 minutes)

          progress("stopping container ")
          // t=600  grace period of 10 min before a kill
          val responset = docker.call(s"/containers/$id/stop", body = Some(emptyJsonObject), queryParams = Map("t" -> "600"))

          progress("deleting container ")
          val responsew = docker.call(s"/containers/$id/wait", body = Some(emptyJsonObject))
          // TODO v=1 ??
          docker.call(s"/containers/$id", method = "delete")

          emptyJsonObject
      }
    }

    requestCmd match {
      case "versions" =>
        val serviceName = jgetString(request, "service")
        val groupName = jgetString(request, "group")
        val j1 = if (useRegistry) {
          val j = repo.call(s"/v1/repositories/$groupName/$serviceName/tags")
          jgetObject(j) map {
            case (k, v) => k
          }
        } else {
          val i = images(serviceName, groupName)
          i map {
            case x => jgetString(x, "version")
          }
        }
        //log.info(noId, JsonObject("versions" -> j1, "i"->i1))
        JsonObject("versions" -> j1.toSeq)
      case "downloads" =>
        log.info(noId, request)
        val serviceName = jgetString(request, "service")
        val groupName = jgetString(request, "group")
        val imagesJson = images(serviceName, groupName)
        JsonObject("images" -> imagesJson)
      case "download" =>
        log.info(noId, request)
        delay
        val serviceName = jgetString(request, "service")
        val groupName = jgetString(request, "group")
        val version = jgetString(request, "request", "version")
        val image = s"$registryName/$groupName/$serviceName"
        val report = images(serviceName, groupName)
        val matches = report filter {
          case item => jget(item, "version") == version
        }
        if (matches.size == 0) {
          progress("starting download")
          // not present download
          val qp = Map[String, String]("fromImage" -> image, "tag" -> version, "registry" -> registryName)
          // Note: returns 200 even if not found!
          val result = docker.call("/images/create", method = "post", queryParams = qp, body = Some(emptyJsonObject))
          val report1 = images(serviceName, groupName)
          val matches1 = report1 filter {
            case item => jget(item, "version") == version
          }
          if (matches1.size == 0) {
            progress("can't find image", level="error")
          } else {
            progress("image downloaded")
          }
          emptyJsonObject
        } else {
          progress("image already present")
          emptyJsonObject
        }
      case "info" =>
        val a = getVersionStatus(container("service-agent", "platform").getOrElse(emptyJsonObject))
        val l = getVersionStatus(container("flume-logger", "service").getOrElse(emptyJsonObject))
        val t = getVersionStatus(container("tools", "platform").getOrElse(emptyJsonObject))
        val v = try {
          System.getenv("COREOS_VERSION")
        } catch {
          case ex: Throwable => ""
        }
        val result = JsonObject("agent" -> a, "logger" -> l, "tools" -> t, "coreos" -> v)
        log.info(noId, JsonObject("info" -> result))
        result
      case "status" =>
        log.info(noId, request)
        val lbStatusF: Future[Json] =
          if (jgetBoolean(request, "request", "hasLB"))
            clientsOpt match {
              case Some(Clients(Some(lb), _)) => lb.queryStatus()
              case _ =>
                log.warn(noId, "Load balancer not initialized; cannot query status")
                Future.successful(emptyJsonObject)
            }
          else Future.successful(emptyJsonObject) // no load balancer
        delay
        val groupName = jgetString(request, "group")
        val svcName = jgetString(request, "service")
        val result = container(svcName, groupName) match {
          case Some(containerJson) =>
            //log.info(noId, containerJson)
            val response = JsonObject("status" -> jgetString(containerJson, "status"),
              "version" -> jgetString(containerJson, "version"))
            //log.warn(noId, containerJson)
            val imageName = jgetString(containerJson, "image")
            image(imageName) match {
              case Some(imageJson) =>
                //log.info(noId, imageJson)
                JsonObject("ports" -> jgetArray(imageJson, "ports")) ++ response
              case None => response
            }
          case None => JsonObject("status" -> "not running")
        }
        val lbStatusOpt =
          scala.util.control.Exception.catching(classOf[TimeoutException], classOf[InterruptedException]).opt {
            Await.result(lbStatusF, 10.seconds) // TODO: refactor the flow and remove the wait
          }
        result ++ JsonObject("agent" -> BuildInfo.version, "lb" -> lbStatusOpt.getOrElse("none"))
      case "deploy" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        val serviceName = jgetString(request, "service")
        val newVersion = jgetString(request, "request", "version")
        //val info = JsonObject("service" -> serviceName, "group" -> groupName,
        //  "version" -> newVersion,
        //  "hasLB" -> jgetBoolean(request, "request", "hasLB"), "select" -> jgetArray(request, "request", "select"))
        container(serviceName, groupName) match {
          case Some(containerJson) =>
            val oldVersion = jgetString(containerJson, "version")
            if (oldVersion == newVersion) {
              progress("already deployed")
              emptyJsonObject
            } else {
              // TODO download if not present
              halt(serviceName, groupName)
              //run(info)
              run(request)
            }
          case None =>
            // TODO download if not present
            //run(info)
            run(request)
        }
      case "provision" =>
        log.info(noId, request)
        delay
        val serviceName = jgetString(request, "request", "serviceName")
        val envName = jgetString(request, "request", "envName")
        val providerName = jgetString(request, "request", "provider")
        val region = jgetString(request, "request", "region")
        val zone = jgetString(request, "request", "zone")
        val instanceType = jgetString(request, "request", "instanceType")
        val count = jgetInt(request, "request", "count")
        val loadBalancerPort = jgetInt(request, "request", "loadBalancerPort")
        val domain = jgetString(request, "request", "domain")
        val instanceRequest = InstanceRequest(providerName, region, zone, envName, serviceName, instanceType, domain, count, config.getString("wp.service-agent.agentVersionInCloudConfig"))
        val orchestrator = system.actorOf(Props[Orchestrator])
        val p = Promise[Orchestrator.PromisedType]()
        orchestrator ! BuildService(serviceName, loadBalancerPort, instanceRequest, p)
        val result = Await.result(p.future, 4.minutes)
        JsonObject("servers" -> result.map((ip) => Map("server" -> ip)))
//      case "provisionServers" =>
//        log.info(noId, request)
//        delay
//        val serviceName = jgetString(request, "request", "serviceName")
//        val envName = jgetString(request, "request", "envName")
//        val providerName = jgetString(request, "request", "provider")
//        val region = jgetString(request, "request", "region")
//        val zone = jgetString(request, "request", "zone")
//        val instanceType = jgetString(request, "request", "instanceType")
//        val count = jgetInt(request, "request", "count")
//        val domain = jgetString(request, "request", "domain")
//        val instance = InstanceRequest(providerName, region, zone, envName, serviceName, instanceType, domain, count, config.getString("wp.service-agent.agentVersionInCloudConfig"))
//
//        ProvisioningControl.getProvider(providerName) match {
//          case Some(provider) =>
//            progress(s"provisioning $count new server(s) with request $instance")
//            val f = provider.create(instance).map((result) => result.nodes.toSeq)
//            val createdNodes = Await.result(f, 4.minutes)
//            JsonObject("serverAddresses" -> createdNodes.map { (server) => server.getPrivateAddresses.asScala.toSeq.head})
//          case None =>
//            log.warn(noId, "provider not found")
//            emptyJsonObject
//        }

      case "halt" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        val serviceName = jgetString(request, "service")
        halt(serviceName, groupName)
      case "shutdown" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        val serviceName = jgetString(request, "service")
        halt(serviceName, groupName, false)
      case "bounce" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        val serviceName = jgetString(request, "service")
        val (name, info) = StateFile.getFirst(Some(reportProgress))
        if (name == "") {
          progress("nothing running")
          emptyJsonObject
        } else {
          halt(serviceName, groupName, clear = false)
          run(jgetObject(info))
          emptyJsonObject
        }
      case x =>
        throw BadInputException(JsonObject("msg" -> "unrecognized command", "cmd" -> x))
    }
  }

  private[this] var busy: String = ""
  private[this] val busyRequests = Seq[String]("download", "deploy", "docmd", "bounce")

  private[this] var mustShutdown = false

  private def shutdown() {
    val (serviceName, info) = StateFile.getFirst()
    if (serviceName != "") {
      def report(s: String, j: Json) {}
      doCmd("shutdown", jgetObject(info), report)
    }
  }

  def receive = {
    case Free =>
      if (mustShutdown) {
        log.error(noId, "Delayed shutdown")
        shutdown()
        service.stop()
      } else {
        busy = ""
      }

    case ShutDown =>
      if (busy == "") {
        log.error(noId, "Immediate shutdown")
        shutdown()
        service.stop()
      } else {
        log.error(noId, "Schedule shutdown")
        mustShutdown = true
      }
    case s: String =>
      //log.info(noId, s)
      val sender1 = sender()
      //val (ok, requestCmd, request) = try {
      val j = Json(s)
      val requestCmd = jgetString(j, "cmd")
      val request = jget(j, "request")
      val id = jgetLong(j, "id")

      def report(cmd: String, response: JsonObject): Unit = {
        val r = JsonObject("cmd" -> cmd, "response" -> response, "id" -> id)
        sender1 ! Compact(r)
      }


      val f1: Future[JsonObject] = {
        val busyRequest = busyRequests.contains(requestCmd)
        if (busy != "" && busyRequest) {
          Future.failed(NotAvailableException(JsonObject("kind" -> "busy", "cmd" -> busy)))
        } else {
          if (busyRequest) busy = requestCmd
          val fcmd = Future {
            val v = doCmd(requestCmd, jgetObject(j), report)
            //if (busyRequest) self ! Free
            v
          }
          fcmd.onComplete {
            case x => if (busyRequest) self ! Free
          }
          fcmd
        }
      }
      f1 onFailure {
        case ex: Throwable =>
          val response = ex match {
            case na: NotAvailableException =>
              if (jgetString(na.msg, "kind") == "busy") {
                val bcmd = jgetString(na.msg, "cmd")
                s"agent is busy running $bcmd"

              } else {
                val client = jgetString(na.msg, "client")
                val where = if (client == "repo") "docker repo" else "docker"
                s"agent reports $where not available"
              }
            case ct: ClientTimeoutException =>
              val client = jgetString(ct.msg, "client")
              val where = if (client == "repo") "docker repo" else "docker"
              s"agent reports $where request timed out"
            case cf: ClientFailException =>
              log.error(noId, "agent internal error", cf)
              val client = jgetString(cf.msg, "client")
              val where = if (client == "repo") "docker repo" else "docker"
              s"agent internal error: failing request to $where"
            case f: FrameworkException => Compact(f.msg)
            case s: SystemException => Compact(s.info)
            case x: Throwable => x.toString
          }
          log.error(noId, JsonObject("requestCmd" -> requestCmd, "request" -> request, "fail" -> response), ex)
          report("fail", JsonObject("msg" -> response))
      }
      f1 foreach {
        case response =>
          report("done", response)
      }
  }
}


