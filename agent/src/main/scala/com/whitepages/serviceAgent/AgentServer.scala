package com.whitepages.serviceAgent

import akka.actor.ActorRef
import com.whitepages.framework.util.ActorSupport
import com.whitepages.info.service_agent.BuildInfo
import scala.concurrent.{Future, Await, Promise, ExecutionContext}
import com.persist.JsonOps._
import com.whitepages.framework.logging.noId
import com.whitepages.framework.exceptions._
import com.persist.Exceptions.SystemException
import Requests._
import scala.concurrent.duration._
import scala.language.postfixOps


class AgentServer(agentHost: String, svcHost: String, lcActor: ActorRef) extends ActorSupport {

  private[this] val (stateName, state) = StateFile.getFirst()
  if (stateName != "") {
    val svcPort = jgetInt(state, "svcPort")
    val hasLB = jgetBoolean(state, "lb")
    lcActor ! LcInit(hasLB, svcPort)
  }

  case object Free

  private[this] val registryName =  config.getString("wp.service-agent.registry")
  private[this] val useRegistry =  config.getBoolean("wp.service-agent.useRegistry")
  private[this] implicit val ec: ExecutionContext = context.dispatcher
  private[this] val docker = DockerClient("docker", this.context)
  private[this] val repo = if (useRegistry) DockerClient("repo", this.context) else null

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

  def trimRepo(s: String, groupName: String): String = {
    val prefix = s"$registryName/$groupName/"
    if (s.startsWith(prefix)) {
      s.replaceFirst(prefix, "")
    } else {
      s
    }
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
        trimRepo(jgetString(item, "fullService"), groupName) == serviceName
    }
    val report1 = report map {
      case d => jgetObject(jdelete(d, "fullService"))
    }
    report1
  }

  def image(name: String): Option[JsonObject] = {
    try {
      //log.info(noId, name)
      val imageJson = docker.call(s"/images/$name/json")
      //log.info(noId, imageJson)
      val config = jget(imageJson, "Config")
      val ports = jgetObject(config, "ExposedPorts") map {
        case (name, value) =>
          name.split("/")(0).toInt
      }
      Some(JsonObject("ports" -> ports.toSeq))
    } catch {
      case ex: Throwable => None // can't find image
    }
  }

  def container(groupName: String): Option[JsonObject] = {
    val r1 = docker.call("/containers/json")
    //log.info(noId, r1)
    val containersJson = jgetArray(r1)
    if (containersJson.size == 0) {
      None
    } else {
      val containerJson = jget(containersJson, 0)
      val image = jgetString(containerJson, "Image")
      val stat = jgetString(containerJson, "Status")
      val id = jgetString(containerJson, "Id")
      val parts = trimRepo(jgetString(containerJson, "Image"), groupName).split(":")
      val j = Some(JsonObject("image" -> image, "version" -> parts(1),
        "status" -> stat, "id" -> id))
      j
    }
  }

  def doCmd(sender: ActorRef, requestCmd: String, request: JsonObject,
            report: (String, JsonObject) => Unit): JsonObject = {

    def reportProgress(p: Progress) {
      report("progress", JsonObject("msg" -> p.msg, "level" -> p.level) ++ p.extra)
    }

    def progress(msg: String, level: String = "info") {
      reportProgress(Progress(msg, level = level))
    }

    def delay {
      report("delay", emptyJsonObject)
    }

    def run(request: JsonObject): JsonObject = {
      val serviceName = jgetString(request, "service")
      val groupName = jgetString(request, "group")
      val version = jgetString(request, "version")
      val hasLB = jgetBoolean(request, "hasLB")
      val select = jgetArray(request, "select") map (jgetString(_))
      val imageName = s"$registryName/$groupName/$serviceName:$version"

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
          val svcPorts = ports filter {
            case port => jgetInt(port) / 10000 == 3
          }
          val lifecyclePort = svcPorts.headOption match {
            case Some(p) => jgetInt(p)
            case None => 39999 // TODO this is an error
          }

          val env = Compact(JsonObject("agentHost" -> agentHost, "containerHost" -> svcHost,
            "lifecyclePort" -> lifecyclePort, "configSelect" -> select))
          val responsej = docker.call("/containers/create", body = Some(JsonObject("Image" -> imageName,
            "Env" -> JsonArray(s"serviceInfo=$env"))))
          val id = jgetString(responsej, "Id")

          StateFile.set(serviceName, jgetObject(request) ++
            JsonObject("svcHost" -> svcHost, "lifecyclePort" -> lifecyclePort, "agentHost" -> agentHost), reportProgress = Some(reportProgress))
          val p = Promise[LcResult]()
          lcActor ! LcInit(hasLB, lifecyclePort)
          lcActor ! LcRun(p, reportProgress, imageName, select)

          progress("starting container")
          val portSpecs = ports map {
            case port => (s"$port/tcp", JsonArray(JsonObject("HostIp" -> "0.0.0.0", "HostPort" -> port.toString)))
          }
          // Use NetworkMode = host until akka can listen on 0.0.0.0
          val drequest = JsonObject("PortBindings" -> portSpecs.toMap, "NetworkMode" -> "host")
          val responset = docker.call(s"/containers/$id/start", body = Some(drequest))

          Await.result(p.future, 5 minutes)
          emptyJsonObject
        //}
      }
    }

    def halt(groupName: String, clear: Boolean = true): JsonObject = {
      container(groupName) match {
        case None =>
          progress("no container currently running")
          emptyJsonObject
        case Some(containerJson) =>
          //log.warn(noId, containerJson)
          val (svcName, info) = StateFile.getFirst(Some(reportProgress))
          if (svcName != "" && clear) StateFile.clear(svcName, Some(reportProgress))
          val id = jgetString(containerJson, "id")
          //val cname = jgetString(container, "name")
          val image = jgetString(containerJson, "image")

          val p = Promise[LcResult]()
          lcActor ! LcStop(p, reportProgress, image)
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
        log.info(noId, request)
        val serviceName = jgetString(request, "service")
        val groupName = jgetString(request, "group")
        val j1 = if (useRegistry) {
          val j = repo.call(s"/v1/repositories/$groupName/$serviceName/tags")
          jgetObject(j) map {
            case (k, v) => k
          }
        } else {
          val i = images(serviceName, groupName)
	  log.info(noId, i)
          i map {
            case x => jgetString(x, "version")
          }
        }
        //log.info(noId, JsonObject("versions" -> j1, "i"->i1))
        JsonObject("versions" -> j1.toSeq)
      case "downloads" =>
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
          val result = docker.call("/images/create", method = "post", queryParams = qp, body = Some(emptyJsonObject))
          progress("image downloaded")
          emptyJsonObject
        } else {
          progress("image already present")
          emptyJsonObject
        }
      case "status" =>
        delay
        val serviceName = jgetString(request, "service")
        val groupName = jgetString(request, "group")
        val state = StateFile.get(serviceName)
        val hasLb = jgetBoolean(state, "hasLB")
        val lb: String = if (!hasLb) {
          "none"
        } else {
          LoadBalancer.status(serviceName)
        }
        val result = container(groupName) match {
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
        result ++ JsonObject("agent" -> BuildInfo.version, "lb" -> lb)
      case "deploy" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        //("deploy", "test")
        val newVersion = jgetString(request, "request", "version")
        val info = JsonObject("service" -> jgetString(request, "service"), "group"->groupName,
          "version" -> newVersion,
          "hasLB" -> jgetBoolean(request, "request", "hasLB"), "select" -> jgetArray(request, "request", "select"))
        container(groupName) match {
          case Some(containerJson) =>
            val oldVersion = jgetString(containerJson, "version")
            if (oldVersion == newVersion) {
              progress("already deployed")
              emptyJsonObject
            } else {
              // TODO download if not present
              halt(groupName)
              run(info)
            }
          case None =>
            // TODO download if not present
            run(info)
        }
      case "halt" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        halt(groupName)
      case "bounce" =>
        log.info(noId, request)
        delay
        val groupName = jgetString(request, "group")
        val (name, info) = StateFile.getFirst(Some(reportProgress))
        if (name == "") {
          progress("nothing running")
          emptyJsonObject
        } else {
          halt(groupName, clear = false)
          run(jgetObject(info))
          emptyJsonObject
        }
      case x =>
        throw BadInputException(JsonObject("msg" -> "unrecognized command", "cmd" -> x))
    }
  }

  private[this] var busy: String = ""
  private[this] val busyRequests = Seq[String]("download", "deploy", "halt", "bounce")

  def receive = {
    case Free =>
      busy = ""
    case s: String =>
      //log.info(noId, s)
      val sender1 = sender
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
            val v = doCmd(sender1, requestCmd, jgetObject(j), report)
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
            case x: Throwable => x.toString()
          }
          log.error(noId, JsonObject("requestCmd" -> requestCmd, "request" -> request, "fail" -> response))
          report("fail", JsonObject("msg" -> response))
      }
      f1 foreach {
        case response =>
          report("done", response)
      }
  }
}


