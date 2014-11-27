package com.whitepages.sampleSvc

import akka.actor.ActorRefFactory
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import com.whitepages.generated.{Response, Test}
import com.whitepages.framework.util.ClassSupport
import scala.language.postfixOps
import com.whitepages.framework.service.ThriftService._
import com.whitepages.framework.service.ThriftService

object SampleActs {

  def testcmd(request: Test.testcmd$args): Test.testcmd$result = {
    val r = Response("2625", "King")
    Test.testcmd$result(Some(r))
  }

}

class SampleHandler(actorFactory: ActorRefFactory) extends Handler with ClassSupport {
  private[this] implicit val timeout = Timeout(10 seconds)
  private[this] implicit val ec = actorFactory.dispatcher

  monitor ! "foo"

  def act(hr: Request): Future[ThriftService.Response] = {
    val f = (hr.cmd, hr.request) match {

      case ("testcmd", request: Test.testcmd$args) =>
        Future {
          log.trace(hr.requestId,"TRACE1")
          monitor !("test", "testcmd")
          val r = SampleActs.testcmd(request)
          log.trace(hr.requestId,"TRACE2")
          r
        }

      case ("testcmd1", request: Test.testcmd1$args) =>
        Future {
          log.error(hr.requestId, "testcmd1 NYI")
          throw new Exception("testcmd1 NYI")
        }

    }
    f map {
      result => ThriftService.Response(result)
    }
  }
}

object SampleHandlerFactory extends HandlerFactory {

  override def start(actorFactory: ActorRefFactory): Handler = new SampleHandler(actorFactory)

}
