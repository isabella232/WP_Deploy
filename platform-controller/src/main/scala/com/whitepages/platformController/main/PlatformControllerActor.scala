package com.whitepages.platformController.main

import akka.pattern._
import akka.util.Timeout
import com.whitepages.framework.exceptions
import com.whitepages.framework.exceptions.BadInputException
import com.whitepages.framework.logging.{noId, RequestId}
import com.whitepages.framework.util.ActorSupport
import com.persist.JsonOps._
import com.whitepages.platformController.commands.CmdHandler
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

class PlatformControllerActor(cmdHandler: CmdHandler) extends ActorSupport {

  override def receive: Receive = {
    case s: String =>
      val sender1 = sender
      // TODO catch json errors
      val incomingJson = jgetObject(Json(s))
      // log.error(noId, s)
      val trackingId = jgetString(incomingJson, "trackingId")
      val spanId = jgetString(incomingJson, "spanId")
      val requestId = if (trackingId == "") {
        RequestId()
      } else {
        RequestId(trackingId, spanId)
      }
      val user0 = jgetString(incomingJson, "user")
      val user = if (user0 == "") {
        log.warn(requestId, "no user specified")
        "???"
      } else {
        user0
      }
      val extra = JsonObject("trackingId" -> requestId.trackingId, "spanId" -> requestId.spanId)
      sender1 ! Compact(JsonObject("cmd" -> "ack") ++ extra)
      def progress(jp: JsonObject): Unit = {
        sender1 ! Compact(jp ++ extra)
      }
      def query(name: String, vals: Seq[String]): String = {
        implicit val timeout = Timeout(2 minutes)
        val v = JsonObject("cmd" -> "query", "name" -> name, "vals" -> vals)
        val f = sender1 ? Compact(v ++ extra)
        val r = try {
          Await.result(f, 2 minutes)
        } catch {
          case ex:TimeoutException => throw BadInputException("timeout while waiting for user input")
        }
        r match {
          case s: String => s
          case x: Any => throw BadInputException(s"value is not a string:${r.toString}")
        }
      }
      try {
        val result = cmdHandler.doCmd(requestId, user, incomingJson, progress, Some(query))
        sender1 ! Compact(JsonObject("cmd" -> "done", "result" -> result) ++ extra)
      } catch {
        case ex: Throwable =>
          //progress(JsonObject("cmd" -> "progress", "msg"->ex.getMessage, "level" -> "error"))
          //log.error(requestId, JsonObject("remote actor exception" -> j), ex)
          val exname = ex.getClass.toString.split("[.]").last
          sender1 ! Compact(JsonObject("cmd" -> "fail", "msg"->ex.getMessage, "ex" -> exname) ++ extra)
      }
  }
}
