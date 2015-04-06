package com.whitepages.platformCommon.data

import com.persist.JsonOps._
import com.whitepages.framework.client.HttpClient
import com.whitepages.framework.client.HttpClient.{W3FormUrlencodedBody, JsonBody, TextBody}
import com.whitepages.framework.exceptions.ClientFailException
import com.whitepages.framework.logging.{noId, RequestId}
import com.whitepages.framework.util.ClassSupport
import com.whitepages.platformCommon.data.EtcdClient.KeyNotFoundException

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/*
Enable spray client redirection support via (spray.can.host-connector.max-redirects)
(see http://spray.io/documentation/1.2.2/spray-can/http-client/#redirection-following)
 */

object EtcdClient {

  private val apiPath = "/v2"

  def pathForKey(prefix: String, key: String): String =
    s"$apiPath/keys/$prefix/$key"

  private val dataPrefix = "DATA"

  def dataPathForKey(prefix: String, key: String): String =
    s"$apiPath/keys/$prefix/$key/$dataPrefix"

  case class KeyNotFoundException(msg: String) extends Exception(msg)

}

/*
 Invariants:
 - Leaf nodes (i.e., nodes with a value) have a EtcdClient.dataPrefix parent
 - Non-leaf nodes are holders
 */
case class EtcdClient(prefix: String = "com.whitepages") extends ClassSupport {
  private val myName = "etcd-client"
  private val awaitDuration = 3500.millis

  def stop(): Future[Unit] = client.stop

  def ping(): Boolean = {
    val pingF = call("/version")
      .map(_ => true)
      .recover { case _: Throwable => false}
    Await.result(pingF, awaitDuration)
  }

  def has(key: String): Boolean = {
    val path = EtcdClient.dataPathForKey(prefix, key)
    val getF =
      call(path, queryParams = Map("consistent" -> "true", "recursive" -> "false"))
        .map(_ => true)
        .recover {
        case nfe: KeyNotFoundException =>
          false
        case other: ClientFailException =>
          throw other
      }
    Await.result(getF, awaitDuration)
  }

  /*
  Pulls value from DATA
  */
  def get(key: String): JsonObject = {
    val getF =
      call(EtcdClient.dataPathForKey(prefix, key), queryParams = Map("consistent" -> "true", "recursive" -> "false"))
        .map(j => jgetObject(Json(jgetString(j, "node", "value"))))
    Await.result(getF, awaitDuration)
  }

  /*
  Pulls value from DATA
  */
  def put(key: String, v: JsonObject): Unit = {
    val path = EtcdClient.dataPathForKey(prefix, key)
    //formatter:off
    val putF =
      call( path
          , kvsOpt = Some(Map( "value" -> Compact(v)))
          , method = "put"
          , queryParams = Map("consistent" -> "true")
          )
        .map(j => {
          log.debug(noId, j)
          (): Unit
        })
    //formatter:on
      Await.result(putF, awaitDuration)
  }

  /*
  Recursive, deletes the directory and keys
  */
  def delete(key: String): Unit = {
    val path = EtcdClient.pathForKey(prefix, key)
    val deleteF =
      call(path, method = "delete", queryParams = Map("recursive" -> "true")).map(_ => (): Unit)
    Await.result(deleteF, awaitDuration)
  }

  /*
  Filters out the child named EtcdClient.dataPrefix
   */
  def children(key: String): Seq[String] = {

    def extractChildren(ja: JsonArray): Seq[String] =
      ja.map(n => jgetString(n, "key").tail)

    val lsF =
      call(EtcdClient.pathForKey(prefix, key), queryParams = Map("recursive" -> "false"))
        .map(json =>
          if (jhas(json, "node", "dir"))
            extractChildren(jgetArray(json, "node", "nodes")).map(s => s.split('/').last)
          else
            Seq.empty[String])
        .recover { case _: KeyNotFoundException => Seq.empty[String] }
    Await.result(lsF, awaitDuration)
  }

  private[this] implicit val ec: ExecutionContext = system.dispatcher
  private[this] val hr = HttpClient.Request("/")
  private[this] val client = HttpClient(system, myName, hr)

  protected def call(url: String, kvsOpt: Option[Map[String, String]] = None, method: String = "get", queryParams: Map[String, String] = Map.empty): Future[Json] = {

    def statusCheck(httpStatus: Int): Unit = {
      if (httpStatus == 404)
        throw KeyNotFoundException(url)
      else if (httpStatus < 200 || httpStatus > 299)
        throw ClientFailException(JsonObject("client" -> "service", "msg" -> "bad status", "status" -> httpStatus))
    }
    val formEncodedBodyOpt = kvsOpt.map(kv => W3FormUrlencodedBody(kv))
    val request = HttpClient.Request(url, method = method, body = formEncodedBodyOpt, queryParams = queryParams)
    client.callHttp(request, RequestId()) map (response => {
      statusCheck(response.status)
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
          throw ClientFailException(JsonObject("client" -> myName, "msg" -> "no body", "url" -> url))
      }
    })
  }

}
