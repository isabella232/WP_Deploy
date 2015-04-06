package com.whitepages.serviceAgent

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.gracefulStop
import akka.util.Timeout
import com.persist.JsonOps._
import com.whitepages.framework.logging.noId
import com.whitepages.framework.service.JsonService
import com.whitepages.framework.service.JsonService._
import com.whitepages.framework.util.ClassSupport

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class AgentHandler(actorFactory: ActorRefFactory, service:JsonService) extends Handler with ClassSupport {
  private[this] implicit val timeout = Timeout(10 seconds)
  private[this] implicit val ec = actorFactory.dispatcher
  private[this] var agentRef: ActorRef = null

  def act(hr: Request): Future[Response] = {
    val f = (hr.cmd, hr.request) match {

      case (cmd: String, r: Json) =>
        Future {
          // for testing
          //def ServiceMessage(cmd: String) = Compact(JsonObject("cmd" -> cmd))
          r //just echo
        }
    }
    f map {
      case result => Response(result)
    }
  }

  override def startApplication(factory: ActorRefFactory) = {
    Future {
      log.info(noId, "Starting Agent")
      val agentHost = config.getString("akka.remote.netty.tcp.hostname")
      val svcHost0 = config.getString("wp.service-agent.containerHost")
      val svcHost = if (svcHost0 == "*") agentHost else svcHost0
      log.info(noId, JsonObject("agent" -> agentHost, "svc" -> svcHost))
      agentRef = factory.actorOf(AgentServer.getProps(agentHost, svcHost, service), name = "agent")
      //log.error(noId, agentRef.toString())
    }
  }

  override def close() = {
    log.info(noId, "Stopping Agent")
    Future {
      if (agentRef != null) {
        val f1 = gracefulStop(agentRef, 2 minutes)
        Await.result(f1, 2 minutes)
      }
    }
  }
}

class AgentHandlerFactory(service:JsonService) extends HandlerFactory {

  override def start(actorFactory: ActorRefFactory): Handler = {
    new AgentHandler(actorFactory, service)
  }

}
