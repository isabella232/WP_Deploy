package com.whitepages.platformController.client

import akka.actor._
import com.persist.JsonOps._
import com.whitepages.framework.logging.RequestId
import com.whitepages.framework.util.ActorSupport
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}
import scala.language.postfixOps
import scala.util.Random
import AgentRequests._


class AgentClient(serviceName: String, groupName:String, envName:String, subName:String, host: String, multi: ActorRef, progress: Option[(Progress) => Unit]) extends ActorSupport {
  //("agent=" + s"akka.tcp://service-agent@$host:8991/user/agent")
  private[this] implicit val ec: ExecutionContext = context.dispatcher
  private[this] val agent = context.actorSelection(s"akka.tcp://service-agent@$host:8991/user/agent")
  private[this] var expectId: Long = 0L
  private[this] var expectP: Option[Promise[JsonObject]] = None
  private[this] var timer: Option[Cancellable] = None
  private[this] var timerExpectId: Long = 0L
  //private[this] val user = System.getProperty("user.name")

  private[this] var seed: Long = 0L

  def nextId: Long = {
    Random.nextLong()
  }

  def finish(result: JsonObject, fail: Boolean) {
    expectP map {
      case p =>
        timer map (_.cancel())
        timerExpectId = 0L
        //expectCmd = ""
        expectId = 0L
        expectP = None
        val f = if (fail) JsonObject("fail" -> true) else emptyJsonObject
        val r1 = result ++ f
        p.trySuccess(r1)
    }
  }

  def report(msg: String, level: String = "info"): Unit = {
    progress foreach {
      case prog => prog(Progress(host, msg, level))
    }
  }

  def receive = {
    case AgentRequest(id:RequestId, cmd: String, request: JsonObject, user:String, p: Promise[JsonObject]) =>
      //expectCmd = cmd
      expectId = nextId
      expectP = Some(p)
      val spanId = Random.nextLong().toHexString

      val r = JsonObject("id" -> expectId, "cmd" -> cmd, "request" -> request,
        "service" -> serviceName, "group"->groupName, "env"->envName, "sub"->subName, "user" -> user,
        "trackingId" -> id.trackingId, "spanId" -> spanId)
      //log.warn(noId, r)
      agent ! Compact(r)
      timerExpectId = expectId
      timer = Some(context.system.scheduler.scheduleOnce(10 seconds) {
        self ! AgentTimer(timerExpectId)
      })
    case AgentTimer(id: Long) =>
      if (timerExpectId == id) {
        report("agent not responding", "error")
        finish(JsonObject("fail" -> "agent not responding"), true)
      }
    case s: String =>
      val j = jgetObject(Json(s))
      //log.error(noId, j)
      val cmd = jgetString(j, "cmd")
      val id = jgetLong(j, "id")
      val response = jgetObject(j, "response")
      if (id == expectId) {
        if (cmd == "progress") {
          val cmd = jgetString(response, "msg")
          if (cmd == "warmPercent") {
            // for now, don't report percents
            val percent = jgetInt(response, "percent")
            report(s"warmup percent: $percent")
          } else {
            val level = jgetString(response, "level")
            if (level != "") {
              report(jgetString(response, "msg"), level = level)
            } else {
              report(jgetString(response, "msg"))
            }
          }
        } else if (cmd == "done") {
          finish(response, false)
        } else if (cmd == "fail") {
          report(jgetString(response, "msg"), "error")
          finish(response, true)
        } else if (cmd == "delay") {
          // wait longer for slow commands after verifying that agent is alive
          timer map (_.cancel())
          timerExpectId = nextId
          timer = Some(context.system.scheduler.scheduleOnce(4 minutes) {
            self ! AgentTimer(timerExpectId)
          })
        } else {
          report(s"unexpected agent client response cmd: $s", "error")
        }
      }
    case x => report(s"bad agent client response: ${x.toString}", "error")
  }

}
