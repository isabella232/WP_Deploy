package com.whitepages.platformController.client

import akka.actor.ActorRefFactory
import com.persist.JsonOps._
import com.whitepages.framework.client.HttpClient
import com.whitepages.framework.client.HttpClient.{JsonBody, TextBody}
import com.whitepages.framework.exceptions.ClientFailException
import com.whitepages.framework.logging.RequestId
import com.whitepages.framework.util.ClassSupport

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

case class DockerClient(service: String, factory: ActorRefFactory) extends ClassSupport {
  private[this] implicit val ec: ExecutionContext = factory.dispatcher
  private[this] val hr = HttpClient.Request("/")
  private[this] val client = HttpClient(factory, service, hr)

  def call(url: String, body: Option[Json] = None, method: String = "get", queryParams: Map[String, String] = Map.empty): Json = {
    //log.info(noId, JsonObject("url"->url,"method"->method))
    val r = body match {
      case Some(j) =>
        HttpClient.Request(url, method = "post", body = Some(JsonBody(j)), queryParams = queryParams)
      case None =>
        HttpClient.Request(url, method = method, queryParams = queryParams)
    }
    val f = client.callHttp(r, RequestId()) map {
      case response =>
        if (response.status < 200 || response.status > 299) {
          throw ClientFailException(JsonObject("client" -> "service", "msg" -> "bad status", "status" -> response.status))
        }
        response.body match {
          case Some(b) =>
            b match {
              case jb: JsonBody =>
                jb.body
              case tb: TextBody =>
                val txt = tb.body
                try {
                  Json(txt)
                } catch {
                  case ex: Throwable =>
                    JsonObject("text" -> tb.body)
                }
              case x: Any => "???"

            }
          case None =>
            throw ClientFailException(JsonObject("client" -> service, "msg" -> "no body", "url" -> url))
        }
    }
    val x = Await.result(f, 3 minutes)
    x
  }

  def stop: Future[Unit] = {
    client.stop
  }

}
