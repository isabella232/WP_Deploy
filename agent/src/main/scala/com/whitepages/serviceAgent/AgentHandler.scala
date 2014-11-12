package com.whitepages.serviceAgent

import akka.actor.{ActorRef, Props, ActorRefFactory}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.{Promise, ExecutionContext, Future, Await}
import com.whitepages.framework.util.{ActorSupport, ClassSupport}
import scala.language.postfixOps
import com.whitepages.framework.service.JsonService._
import com.persist.JsonOps._
import com.whitepages.framework.logging.{RequestId, noId}
import akka.pattern.gracefulStop
import com.whitepages.framework.exceptions._
import com.persist.Exceptions.SystemException
import com.persist.JsonOps._

class AgentHandler(actorFactory: ActorRefFactory) extends Handler with ClassSupport {
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
      val agentHost = config.getString("akka.remote.netty.tcp.hostname")
      val svcHost0 = config.getString("wp.service-agent.containerHost")
      val svcHost = if (svcHost0 == "*") agentHost else svcHost0
      log.info(noId, JsonObject("agent"->agentHost, "svc"->svcHost))
      val lcActor = system.actorOf(Props(classOf[LifecycleClient], svcHost), name = "lifecycle")
      agentRef = factory.actorOf(Props(classOf[AgentServer], agentHost, svcHost, lcActor), name = "agent")
    }
  }

  override def close() = {
    Future {
      if (agentRef != null) {
        val f1 = gracefulStop(agentRef, 2 minutes)
        Await.result(f1, 2 minutes)
      }
    }
  }
}

object AgentHandlerFactory extends HandlerFactory {

  override def start(actorFactory: ActorRefFactory): Handler = {
    new AgentHandler(actorFactory)
  }

}
