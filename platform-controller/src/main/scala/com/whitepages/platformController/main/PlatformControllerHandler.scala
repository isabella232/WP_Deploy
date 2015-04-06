package com.whitepages.platformController.main

import akka.actor.{Props, ActorRefFactory}
import akka.util.Timeout
import com.persist.JsonOps._
import com.whitepages.framework.exceptions.BadInputException
import com.whitepages.framework.logging.noId
import com.whitepages.framework.service.JsonService._
import com.whitepages.framework.util.ClassSupport
import com.whitepages.platformCommon.data.{JsonServiceData, EtcdServiceData}
import com.whitepages.platformController.client.DockerClient
import com.whitepages.platformController.commands.CmdHandler
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class PlatformControllerHandler(actorFactory: ActorRefFactory,etcd:Boolean) extends Handler with ClassSupport {

  private[this] implicit val timeout: Timeout = Timeout(10 seconds)
  private[this] implicit val ec = actorFactory.dispatcher

  private[this] val sd = if (etcd) {
     EtcdServiceData()
  } else {
    val fname = "config/servers.json"
    val txt = Source.fromFile(fname).getLines.mkString("\n")
    val json = Json(txt)
    JsonServiceData(json)
  }

  private[this] val repo = DockerClient("repo", actorFactory)

  private[this] val cmdHandler = CmdHandler(actorFactory, sd, repo)


  def act(request: Request): Future[Response] = {
    var pseq = Seq[Json]()

    def progress(j: Json) {
      pseq.synchronized {
        pseq = j +: pseq
      }
    }

    val cmd = request.cmd
    val cmdInfo = if (cmd == "") {
      Seq()
    } else {
      val parts = cmd.split("/")
      parts.toSeq map {
        case part =>
          if (part == "") throw BadInputException("empty cmd part")
          val subparts = part.split(":")
          if (subparts.size >= 2) {
            (subparts(0), subparts(1))
          } else {
            ("cmd", subparts(0))
          }
      }
    }

    val cmdObj = cmdInfo.toMap
    val bodyObj = jgetObject(request.request)
    val extra = JsonObject("method" -> request.method)
    val combinedObj = cmdObj ++ bodyObj
    val user0 = jgetString(combinedObj, "user")
    val user = if (user0 == "") {
      //log.warn(request.requestId, "no user specified")
      "???"
    } else {
      user0
    }
    val result = try {
      val r = cmdHandler.doCmd(request.requestId, user, combinedObj ++ extra ++ JsonObject("source" -> "HTTP"), progress, None)
      if (r == emptyJsonArray || r == emptyJsonObject) {
        emptyJsonObject
      } else {
        JsonObject("result" -> r)
      }
    } catch {
      case ex: BadInputException => throw ex // pass it up to framework
      case ex: Throwable =>
        //log.error(request.requestId, JsonObject("http exception" -> (cmdObj ++ bodyObj)), ex)
        JsonObject("fail" -> ex.toString)
    }
    val pp = if (pseq.size == 0) emptyJsonObject else JsonObject("progress" -> pseq.reverse)
    val response = result ++ pp
    Future.successful(Response(response))
  }

  override def startApplication(factory: ActorRefFactory): Future[Unit] = {
    // TODO run actor on different actor system
    val pact = factory.actorOf(Props(classOf[PlatformControllerActor], cmdHandler), name = "controller")
    Future.successful(())
  }
}

class PlatformControllerHandlerFactory(etcd:Boolean) extends HandlerFactory {

  override def start(actorFactory: ActorRefFactory): Handler = new PlatformControllerHandler(actorFactory,etcd)

}
