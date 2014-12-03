/*
package com.whitepages.framework.client

import scala.concurrent.{ExecutionContext, Future}
import spray.http.{StatusCode, HttpEntity}
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import akka.actor.ActorRefFactory
import java.net.URLEncoder
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import com.persist.JsonOps._
import com.whitepages.framework.logging.RequestId
import com.whitepages.framework.util.{ClassSupport, Util}
import spray.http.HttpRequest
import spray.http.HttpResponse

case class Neo4jClient(actorFactory: ActorRefFactory, clientName: String) extends ClassSupport {
  private[this] val config = system.settings.config
  private[this] val cpath = Seq("wp", serviceName, "clients", clientName)
  private[this] val logRequest = config.getBoolean((cpath :+ "logRequest").mkString("."))
  private[this] implicit val ex: ExecutionContext = system.dispatcher

  private[this] val retryInterval: FiniteDuration =
    FiniteDuration(config.getMilliseconds((cpath :+ "retryInterval").mkString(".")), TimeUnit.MILLISECONDS)
  private[this] val jsonMapper = JsonLoggingMapper(clientName)

  private[this] val healthRequest = HttpRequest(
    method = GET,
    uri = "/admin:ping") // TODO change this

  private[this] val httpClient = new BaseHttpClient(actorFactory, clientName, healthRequest, retryInterval)

  def callJson = httpClient.call(jsonMapper) _

  def getJson(cmd: String, data: JsonObject = emptyJsonObject): Future[Json] = {
    //val id = getId(id0)
    //val spanId = Random.nextLong().toHexString
    //val props = //makeUUIDS(id, spanId) ++
    //  JsonObject("cmd" -> cmd,
    //    "method" -> "getJson", "client" -> clientName)
    val params = data map {
      case (name, s: String) => name + "=" + URLEncoder.encode(s, "UTF-8")
      case (name, j) => name + "=(" + URLEncoder.encode(Compact(j), "UTF-8") + ")"
    }
    val q = if (jsize(data) == 0) "" else "?" + params.mkString("&")

    //val headers = makeHeaders(id, spanId)
    val request = HttpRequest(
      method = GET,
      uri = "/" + cmd + q,
      headers = List() // headers
    )
    //httpClient.callJson(request, props, noId)
    callJson(request, ReqIdAndSpanOut(RequestId("", ""), ""), 100, None).map(httpResp => HttpResponseHelpers.httpResponseToJson(httpResp))
  }

  def postJson(cmd: String, data: Json): Future[Json] = {
    //val id = getId(id0)
    //val spanId = Random.nextLong().toHexString
    val req = if (logRequest) JsonObject("request" -> data) else emptyJsonObject
    //val props = //makeUUIDS(id, spanId) ++
    //  JsonObject("cmd" -> cmd,
    //    "method" -> "postJson", "client" -> clientName) ++ req

    //val headers = makeHeaders(id, spanId)
    val request = HttpRequest(
      method = POST,
      uri = "/" + cmd,
      headers = List(), //headers,
      entity = HttpEntity(`application/json`, Compact(data))
    )
    //httpClient.callJson(request, props, noId)
    callJson(request, ReqIdAndSpanOut(RequestId("", ""), ""), 100, None).map(httpResp => HttpResponseHelpers.httpResponseToJson(httpResp))
  }

  def stop:Future[Unit] = {
    httpClient.stop
  }

  object HttpResponseHelpers {
    def httpResponseToJson(response: HttpResponse): Json = response match {
      case HttpResponse(status, entity, _, _) =>
        checkStatus2XX(status)
        val jString = entity.data.asString
        Json(jString)
    }

    def checkStatus2XX(status: StatusCode) {
      if (status.intValue < 200 || status.intValue >= 300) throw new Exception("non-2XX response status code from service")
    }

  }

}
*/
