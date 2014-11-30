package com.whitepages.serviceDeploy.deploy

import akka.actor.{PoisonPill, ActorRef, Props, ActorRefFactory}
import scala.concurrent.{Await, Future, Promise, ExecutionContext}
import com.persist.JsonOps._
import com.whitepages.serviceDeploy.deploy.Requests._
import scala.concurrent.duration._
import scala.language.postfixOps

class Coordinator(factory: ActorRefFactory, serviceName: String, groupName:String, localHost: String, hosts: Seq[String], progress: Option[Progress => Unit]) {
  private[this] implicit val ec: ExecutionContext = factory.dispatcher

  //println("CO="+localHost)

  val hostMap = (hosts map {
    case host: String =>
      val host1 = if (host == "*") localHost else host
      val serverRef = factory.actorOf(Props(classOf[AgentClient], serviceName, groupName, host1, null, progress))
      (host1, serverRef)
  }).toMap

  def fromAll(cmd: String): JsonObject = {
    val ps = hostMap.toSeq map {
      case (host, ref) =>
        val p = Promise[JsonObject]()
        ref ! AgentRequest(cmd, emptyJsonObject, p)
        p.future map {
          case result => (host, result)
        }
    }
    val fs: Future[Seq[(String, JsonObject)]] = Future.sequence(ps)
    val fs1 = fs map {
      case f1 => f1 filter {
        case (n, v) => !jhas(v, "fail")
      }
    }
    Await.result(fs1, 10 minutes).toMap
  }

  def fromFirst(cmd: String): JsonObject = {
    val (host, ref) = hostMap.head
    val p = Promise[JsonObject]()
    ref ! AgentRequest(cmd, emptyJsonObject, p)
    val p1 = p.future map {
      case result => JsonObject(host -> result)
    }
    Await.result(p1, 10 minutes)
  }

  private def downloadOrdered(map: Map[String, ActorRef], serviceName: String, version: String): Future[Unit] = {
    if (map.size > 0) {
      val (host, ref) = map.head
      val map1 = map.tail
      val p = Promise[JsonObject]()
      ref ! AgentRequest("download", JsonObject("version" -> version), p)
      val x = p.future flatMap {
        case result =>
          progress foreach {
            case p =>
              if (jgetBoolean(result, "fail")) {
                p(Progress(host, "download failed", level = "error"))
              } else {
                p(Progress(host, "download complete"))
              }
          }
          downloadOrdered(map1, serviceName, version)
      }
      x
    } else {
      Future.successful(())
    }
  }

  def download(serviceName: String, version: String) {
    val f = downloadOrdered(hostMap, serviceName, version)
    Await.result(f, 10 minutes)
  }


  private def deployOrdered(map: Map[String, ActorRef], serviceName: String, version: String, select: Seq[String], hasLB: Boolean): Future[Unit] = {
    if (map.size > 0) {
      val (host, ref) = map.head
      val map1 = map.tail
      val p = Promise[JsonObject]()
      ref ! AgentRequest("deploy", JsonObject("version" -> version, "select" -> select, "hasLB" -> hasLB), p)
      p.future flatMap {
        case result =>
          progress foreach {
            case p =>
              if (jgetBoolean(result, "fail")) {
                p(Progress(host, "deploy failed", level = "error"))
              } else {
                p(Progress(host, "deployed"))
              }
          }
          deployOrdered(map1, serviceName, version, select, hasLB)
      }
    } else {
      Future.successful(())
    }
  }

  def deploy(serviceName: String, version: String, select: Seq[String], hasLB: Boolean) {
    val f = downloadOrdered(hostMap, serviceName, version)
    val f1 = f flatMap {
      case x => deployOrdered(hostMap, serviceName, version, select, hasLB)
    }
    Await.result(f1, 10 minutes)
  }

  def halt() {
    val ps = hostMap.toSeq map {
      case (host, ref) =>
        val p = Promise[JsonObject]()
        ref ! AgentRequest("halt", emptyJsonObject, p)
        p.future
    }
    val fs: Future[Seq[JsonObject]] = Future.sequence(ps)
    Await.result(fs, 10 minutes)
  }

  private def bounceOrdered(map: Map[String, ActorRef]): Future[Unit] = {
    if (map.size > 0) {
      val (host, ref) = map.head
      val map1 = map.tail
      val p = Promise[JsonObject]()
      ref ! AgentRequest("bounce", emptyJsonObject, p)
      p.future map {
        case result =>
          progress foreach {
            case p =>
              if (jgetBoolean(result, "fail")) {
                p(Progress(host, "bounce failed", level = "error"))
              } else {
                p(Progress(host, "bounced"))
              }
          }
          bounceOrdered(map1)
      }
    } else {
      Future.successful(())
    }
  }

  def bounce() {
    val f = bounceOrdered(hostMap)
    Await.result(f, 10 minutes)
  }


  def close {
    hostMap foreach {
      case (host, ref) =>
        ref ! PoisonPill
    }
  }
}

object Coordinator {
  // TODO reuse if same hosts
  def apply(factory: ActorRefFactory, serviceName: String, groupName:String, localHost: String, hosts: Seq[String], progress: Option[Progress => Unit] = None) =
    new Coordinator(factory, serviceName, groupName,localHost, hosts, progress)
}



