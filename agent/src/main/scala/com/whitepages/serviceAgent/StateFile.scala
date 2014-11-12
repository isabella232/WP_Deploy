package com.whitepages.serviceAgent

import com.persist.JsonOps._
import com.whitepages.serviceAgent.Requests.Progress
import scala.reflect.io.File
import com.whitepages.framework.util._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.language.postfixOps
import com.whitepages.framework.logging.noId

/**
 * This a test of list blocks
 *
 * - foo
 * - bar
 *
 * Thats all!
 */
object StateFile extends ClassSupport {

  private[this] val fileName = "/opt/wp/service-agent/agent.json"
  private[this] val f = File(fileName)

  private def read(reportProgress:Option[(Progress)=>Unit]= None): JsonObject = {
    if (f.exists) {
      val text = try {
        val ftext = FileIO.read(fileName)
        Await.result(ftext, 3 minutes)
      } catch {
        case ex: Throwable =>
          val msg = "Can't read state file"
          log.error(noId, msg, ex)
          reportProgress foreach {
            case p =>
              p(Progress(msg, level = "error"))
          }
          "{}"
      }
      try {
        jgetObject(Json(text))
      } catch {
        case ex: Throwable =>
          val msg = "Bad Json in state file"
          log.error(noId, msg, ex)
          reportProgress foreach {
            case p =>
              p(Progress(msg, level = "error"))
          }
          emptyJsonObject
      }
    } else {
      emptyJsonObject
    }
  }

  private def write(v: JsonObject, reportProgress:Option[(Progress)=>Unit]=None) {
    try {

      val f = FileIO.write(fileName, Pretty(v) + "\n")
      Await.result(f, 3 minutes)
    } catch {
      case ex: Throwable =>
        val msg = "Can't write state file"
        log.error(noId, msg, ex)
        reportProgress foreach {
          case p =>
            p(Progress(msg, level = "error"))
        }
    }
  }

  def set(serviceName: String, value: Json, reportProgress:Option[(Progress)=>Unit]= None) {
    val old = read(reportProgress)
    val n = old + (serviceName -> value)
    write(n, reportProgress)
  }

  def clear(serviceName: String, reportProgress:Option[(Progress)=>Unit]= None) {
    val old = read(reportProgress)
    val n = old - serviceName
    //log.error(noId, JsonObject("old" -> old, "new" -> n, "name" -> serviceName))
    write(n, reportProgress)
  }

  def get(serviceName: String): Json = {
    val old = read()
    jget(old, serviceName)
  }

  def getFirst(reportProgress:Option[(Progress)=>Unit]= None): (String, Json) = {
    val old = read(reportProgress)
    old.toSeq.headOption match {
      case Some((n, v)) => (n, v)
      case None => ("", emptyJsonObject)
    }
  }

}
