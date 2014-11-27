package com.whitepages.sampleApp

import akka.actor.ActorRefFactory
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import com.whitepages.framework.util.ClassSupport
import scala.language.postfixOps
import com.whitepages.framework.service.JsonService._
import com.persist.JsonOps._

class SampleHandler(actorFactory: ActorRefFactory) extends Handler with ClassSupport {
  //private[this] implicit val timeout = Timeout(10 seconds)
  private[this] implicit val ec = actorFactory.dispatcher


  def act(hr: Request): Future[Response] = {
    val f:Future[Json] = (hr.cmd, hr.request) match {

      case ("testcmd", request: Json) =>
        Future {
          JsonObject("in"->request,"out"->"ok")
        }

      case ("testcmd1", request: Json) =>
        Future {
          "goodbye world"
        }
    }
    f map {
      result => Response(result)
    }
  }
}

object SampleHandlerFactory extends HandlerFactory {

  override def start(actorFactory: ActorRefFactory): Handler = new SampleHandler(actorFactory)

}
