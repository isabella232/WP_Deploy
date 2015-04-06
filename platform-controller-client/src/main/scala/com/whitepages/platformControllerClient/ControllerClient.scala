package com.whitepages.platformControllerClient

import java.util.UUID
import akka.actor.{Props, ActorRefFactory, Cancellable, Actor}
import com.persist.JsonOps._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.{Await, ExecutionContext, Promise}
import scala.util.Random
import scala.language.postfixOps

object ControllerClient {

  private[platformControllerClient] case class ControllerRequest(cmd: String,
                                                                 options: JsonObject,
                                                                 p: Promise[JsonObject],
                                                                 progress: (Progress) => Unit,
                                                                 query: (String, Seq[String]) => String)

  private[platformControllerClient] case class ControllerTimer(id: Long)

  case class Progress(host: String, msg: String, level: String = "info")

}

case class ControllerClient(factory: ActorRefFactory, localHost: String, user: String) {

  import ControllerClient._

  private[this] val clientActor = factory.actorOf(Props(classOf[ControllerClientActor], localHost, user), name = "controllerClient")

  def call(cmd: String, options: JsonObject,
           progress: (Progress) => Unit,
           query: (String, Seq[String]) => String): JsonObject = {
    val p = Promise[JsonObject]()
    clientActor ! ControllerRequest(cmd, options, p, progress, query)
    val result = Await.result(p.future, Duration.Inf)
    result
  }
}

private[platformControllerClient] class ControllerClientActor(host: String, user: String) extends Actor {

  // TODO permit multiple simultaneous requests

  private[this] implicit val ec: ExecutionContext = context.dispatcher

  import ControllerClient._

  def nextId: Long = {
    Random.nextLong()
  }

  def finish(result: JsonObject, fail: Boolean) {
    expectP map {
      case p =>
        timer map (_.cancel())
        timerExpectId = 0L
        expectTrackingId = ""
        expectP = None
        val f = if (fail) JsonObject("fail" -> true) else emptyJsonObject
        val r1 = result ++ f
        p.trySuccess(r1)
    }
  }

  def report(msg: String, level: String = "info"): Unit = {
    expectProgress foreach {
      case prog => prog(Progress(host, msg, level))
    }
  }

  private[this] val controller = context.actorSelection(s"akka.tcp://platform-controller@$host:49004/user/controller")

  private[this] var expectTrackingId: String = ""
  private[this] var expectP: Option[Promise[JsonObject]] = None
  private[this] var timer: Option[Cancellable] = None
  private[this] var timerExpectId: Long = 0L
  private[this] var expectProgress: Option[(Progress) => Unit] = None
  private[this] var expectQuery: Option[(String, Seq[String]) => String] = None

  def receive = {
    case ControllerRequest(cmd, options, p, progress, query) =>
      val trackingId = UUID.randomUUID().toString
      val spanId = "0"
      expectTrackingId = trackingId
      expectP = Some(p)
      expectProgress = Some(progress)
      expectQuery = Some(query)
      val j = JsonObject("cmd" -> cmd, "trackingId" -> trackingId,
        "spanId" -> spanId, "user" -> user) ++ options
      controller ! Compact(j)
      timerExpectId = nextId
      timer = Some(context.system.scheduler.scheduleOnce(3 seconds) {
        self ! ControllerTimer(timerExpectId)
      })
    case ControllerTimer(id) =>
      if (id == timerExpectId) {
        report("platform controller not responding", "error")
        finish(JsonObject("fail" -> "platform controller not responding"), true)
      }
    case s: String =>
      val j = jgetObject(Json(s))
      val cmd = jgetString(j, "cmd")
      val trackingId = jgetString(j, "trackingId")
      val response = jgetObject(j, "response")
      if (trackingId == expectTrackingId) {
        if (cmd == "ack") {
          timer map (_.cancel())
          timerExpectId = nextId
          timer = Some(context.system.scheduler.scheduleOnce(4 minutes) {
            self ! ControllerTimer(timerExpectId)
          })
        } else if (cmd == "progress") {
          report(jgetString(j, "msg"), jgetString(j, "level"))
        } else if (cmd == "query") {
          //println(Pretty(j))
          sender ! {
            expectQuery match {
              case Some(query) => query(jgetString(j, "name"), jgetArray(j, "vals").map {
                jgetString(_)
              })
              case None => "???"
            }
          }
        } else if (cmd == "done") {
          finish(j, false)
        } else if (cmd == "fail") {
          finish(j, true)
        } else {
          finish(JsonObject("bad cmd" -> cmd), true)
        }
      }
  }
}
