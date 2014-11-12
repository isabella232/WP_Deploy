package com.whitepages.serviceAgent

import com.whitepages.serviceAgent.Requests.Progress

import scala.language.postfixOps
import scala.sys.process._
import com.whitepages.framework.util.ClassSupport
import com.whitepages.framework.logging.noId
import com.persist.JsonOps._

object LoadBalancer extends ClassSupport {

  def enable(reportProgress: Option[(Progress) => Unit]) {
    try {
      Seq("lbman-client", "enable") !!
    } catch {
      case ex: Throwable =>
        val msg = "LB enable fail"
        log.error(noId, msg, ex)
        reportProgress foreach {
          case p =>
            p(Progress(msg, level = "error"))
        }
    }
  }

  def disable(reportProgress: Option[(Progress) => Unit]) {
    try {
      Seq("lbman-client", "disable") !!
    } catch {
      case ex: Throwable =>
        val msg = "LB disable fail"
        //log.error(noId, msg, ex)
        reportProgress foreach {
          case p =>
            p(Progress(msg, level = "error"))
        }
    }
  }

  def status(service: String): String = {
    try {
      val s = Seq("lbman-client", "-p", s"${service}-pool0", "-s", "/dev/null", "init") !!
      val j = Json(s)
      log.info(noId, j)
      val (s1,v1) = jgetObject(j).head
       val (s2,v2) = jgetObject(v1).head
      jgetString(v2)
    } catch {
      case ex: Throwable => "no response"
    }

  }
}
