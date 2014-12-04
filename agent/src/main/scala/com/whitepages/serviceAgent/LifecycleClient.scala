package com.whitepages.serviceAgent

import com.whitepages.framework.util.ActorSupport
import akka.actor.{Cancellable, ActorSelection}
import scala.concurrent.{ExecutionContext, Promise}
import com.persist.JsonOps._
import com.whitepages.framework.logging.noId
import Requests._
import scala.concurrent.duration._
import scala.language.postfixOps

import scala.concurrent.duration.FiniteDuration

class LifecycleClient(svcHost: String) extends ActorSupport {
  private[this] implicit val ex:ExecutionContext = context.dispatcher

  private[this] val waitRunning = getFiniteDuration("wp.service-agent.waitRunning")
  private[this] val waitWarming = getFiniteDuration("wp.service-agent.waitWarming")
  private[this] val waitUp = getFiniteDuration("wp.service-agent.waitUp")
  private[this] val waitStopping = getFiniteDuration("wp.service-agent.waitStopping")
  private[this] val waitDraining = getFiniteDuration("wp.service-agent.waitDraining")
  private[this] val waitDrained = getFiniteDuration("wp.service-agent.waitDrained")
  private[this] val waitStopped = getFiniteDuration("wp.service-agent.waitStopped")

  private[this] var svcPort: Int = 0
  private[this] var hasLB: Boolean = false
  private[this] var serviceActor: Option[ActorSelection] = None

  private[this] var state: String = ""

  private[this] var runp: Option[Promise[LcResult]] = None
  private[this] var reportProgress: Option[(Progress) => Unit] = None
  private[this] var iname: String = "???"
  private[this] var select: Seq[String] = Seq[String]()

  private def ServiceMessage(cmd: String, request: Option[Json] = None) = {
    val rj = request match {
      case Some(r) => JsonObject("request" -> r)
      case None => emptyJsonObject
    }
    val j = JsonObject("cmd" -> cmd) ++ rj
    Compact(j)
  }

  private def progress(msg: String, level: String = "info", extra: JsonObject = emptyJsonObject) {
    reportProgress foreach {
      case p =>
        p(Progress(msg, level = level, extra = extra))
    }
  }

  private[this] var timer: Option[Cancellable] = None
  private[this] var timerId: Int = 0

  private def expect(cmd: String, wait: FiniteDuration, start: Boolean = false): Unit = {
    if (start || timer != None) {
      timer foreach {
        case t => t.cancel()
      }
      timerId += 1
      timer = Some(system.scheduler.scheduleOnce(wait) {
        self ! LcTimer(cmd, timerId)
      })
    }
  }

  private def done(result: LcResult) {
    runp foreach {
      case p => p.trySuccess(result)
    }
    runp = None
    timerId += 1
    if (timer != None) {
      timer foreach {
        case t => t.cancel()
      }
    }
    timer = None
    reportProgress = None
  }

  def receive = {

    case LcTimer(expect, id) if (id == timerId) =>
      timer foreach {
        case t => t.cancel()
      }
      progress(s"failed expecting $expect", level = "error")
      log.error(noId, JsonObject("msg" -> "lifecycle timeout", "expecting" -> expect))
      done(LcResult(expect))
    case LcInit(hasLB0, svcPort0) =>
      if (svcPort != svcPort0) {
        svcPort = svcPort0
        serviceActor = Some(context.actorSelection(s"akka.tcp://lifecycle@$svcHost:$svcPort/user/lifecycle"))
      }
      hasLB = hasLB0

    case LcRun(p, reportProgress0, iname0, select0) =>
      //     running
      // => init
      //     warming
      //     warmed
      // => onlb
      //     up
      runp = Some(p)
      reportProgress = Some(reportProgress0)
      iname = iname0
      select = select0
      expect("running", waitRunning, start = true)

    case LcStop(p, reportProgress0, iname0) =>
      //     stopping
      // => offlb
      //     draining
      //     drained
      //     stopped
      runp = Some(p)
      reportProgress = Some(reportProgress0)
      iname = iname0
      // signals don't always work, so use explicit stop
      expect("stopping", waitStopping, start = true)
      serviceActor map (_ ! ServiceMessage("stop"))


    case s: String =>
      //log.info(noId,JsonObject("serviceActor"->sender.path.toString))
      val msg = Json(s)
      val cmd = jgetString(msg, "cmd")
      state = cmd
      //log.info(noId, JsonObject("msg" -> msg, "sender" -> sender.path.toString))
      if (cmd == "warmPercent") {
        val percent = jgetInt(msg, "percent")
        progress(cmd, extra = JsonObject("percent" -> percent))
      } else {
        progress(cmd)
      }
      cmd match {
        case "running" =>
          val request = JsonObject("configSelect" -> select)
          expect("warming", waitWarming)
          serviceActor map (_ ! ServiceMessage("init", Some(request)))
        case "warming" =>
          // 125 seconds = 2 mins + 5 secs
          val to = jgetInt(msg, "timeout")
          expect("warmed", to seconds)
          log.info(noId, msg)
        case "warmPercent" =>
          val percent = jgetInt(msg, "percent")
          log.info(noId, msg)
        case "warmed" =>
          expect("up", waitUp)
          if (hasLB) {
            progress("enabling lb")
            LoadBalancer.enable(reportProgress)
          }
          serviceActor map (_ ! ServiceMessage("onlb"))
        case "up" =>
          done(LcResult())
        case "stopping" =>
          expect("draining", waitDraining)
          if (hasLB) {
            progress("disabling lb")
            LoadBalancer.disable(reportProgress)
          }
          serviceActor map (_ ! ServiceMessage("offlb"))
        case "draining" =>
          expect("drained", waitDrained)
        case "drained" =>
          expect("stopped", waitStopped)
        case "stopped" =>
          done(LcResult())
        case x: String =>
          log.error(noId, JsonObject("msg" -> "bad cmd from service", "cmd" -> x))
      }
    case x: Any =>
      log.error(noId, JsonObject("badMsg" -> x.toString))
  }
}
