package com.whitepages.platformCommon.dns

import akka.actor.ActorSystem
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.persist.Exceptions.SystemException
import com.persist.JsonMapper._
import com.persist.JsonOps._
import com.whitepages.platformCommon.credentials.CredentialProvider
import spray.can.Http
import spray.can.Http.HostConnectorSetup
import spray.client.pipelining._
import spray.http._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

// This module provides a programmatic interface to the REST API in front of PowerDNS.
case class DNSRecord(id: Int, name: String, content: String, ttl: Int, `type`: String = "A")

object DNSControl {
  def getDNSRecords(res: Option[Json]): Seq[DNSRecord] = {
    res match {
      case None       => Seq.empty[DNSRecord]
      case Some(json) => jgetArray(json, "hosts").map(each => ToObject[DNSRecord](each))
    }
  }

  def getDNSRecord(res: Option[Json]): Option[DNSRecord] = {
    res match {
      case None => None
      case Some(json) =>
        val es = jgetArray(json)
        if (es.isEmpty) None
        else scala.util.control.Exception.catching(classOf[SystemException]).opt { ToObject[DNSRecord](es.head) }
    }
  }

  def getHostDNSRecord(res: Option[Json]): Option[DNSRecord] = {
    res match {
      case None => None
      case Some(json) =>
        val es = jgetObject(json, "host")
        if (es.isEmpty) None
        else scala.util.control.Exception.catching(classOf[SystemException]).opt { ToObject[DNSRecord](es) }
    }
  }

  val httpConnectTimeout = Timeout(5555.millis)
  val apiPort = 80
  val apiRoot = "/v1"
}

trait DNSControl {
  import DNSControl._

  protected implicit def system: ActorSystem
  protected implicit val ec: ExecutionContext
  def dnsControlHost: String
  def credentialProvider: CredentialProvider

  private [this] implicit val askTimeout = httpConnectTimeout

  private [this] def buildControlUri(path: Uri.Path): Uri =  {
    val authority = Uri.Authority(Uri.Host(dnsControlHost), apiPort)
    Uri(scheme = "http", authority = authority, path = path)
  }

  private [this] def buildPipeline(uri: Uri): Future[SendReceive] = {
    val dnsCredentials = credentialProvider.dnsCredentials()
    for (
      Http.HostConnectorInfo(hostConnector, _) <-
        IO(Http) ? HostConnectorSetup(host = uri.authority.host.address, port = apiPort, sslEncryption = false)
    ) yield (
      addCredentials(BasicHttpCredentials(dnsCredentials.identity, dnsCredentials.secret))
        ~> sendReceive(hostConnector)
    )
  }

  private [this] def pipelineRequest[T](path: Uri.Path, requestBuilder: (Uri => HttpRequest), f: Option[Json] => T): Future[T] = {
    val controlUri = buildControlUri(path)
    val pipeline = buildPipeline(controlUri)
    val request = requestBuilder(controlUri.toRelative)
    pipeline
      .flatMap(_(request))
      .map(
        response => {
          if (response.status.isSuccess) scala.util.control.Exception.catching(classOf[SystemException]).opt { Json(response.entity.asString) }
          else None
        })
      .map(f)
  }

  // List all records defined in $domain, or an empty Seq if it doesn't exist.

  // curl -X GET -H 'Content-Type: application/json' http://pdns-master0.util.pages/v1/dev.pages
  def list(domain: String): Future[Seq[DNSRecord]] = {
    val getRequest = (path: Uri) => HttpRequest(method = HttpMethods.GET, uri = path)
    pipelineRequest(Uri.Path(s"$apiRoot/$domain"), getRequest, getDNSRecords)
  }

  // If an A record exists for $host in $domain, return it; otherwise, return None.

  // curl -u platform-api -X GET -H 'Content-Type: application/json' http://pdns-master0.util.pages/v1/dev.pages/platform-test.dev.pages
  //   [{"content": "10.8.0.16", "id": 69119, "type": "A", "name": "platform-test.dev.pages", "ttl": 3600}]
  def get(host: String, domain: String): Future[Option[DNSRecord]] = {
    val getRequest = (path: Uri) => HttpRequest(method = HttpMethods.GET, uri = path)
    pipelineRequest(Uri.Path(s"$apiRoot/$domain/$host.$domain"), getRequest, getDNSRecord)
  }

  // If an A record exists for $host in $domain, return it; otherwise, create one and return it.

  // curl -u platform-api -X PUT -H 'Content-Type: application/json' -d '{"name": "platform-test.dev.pages", "type": "A"}' http://pdns-master0.util.pages/v1/dev.pages
  //   {"status": "created", "host": {"content": "10.8.0.16", "id": 69119, "type": "A", "name": "platform-test.dev.pages", "ttl": 3600}}
  def getOrCreate(host: String, domain: String): Future[Option[DNSRecord]] = {
    def createNewRecord(host: String, domain: String) = {

      val req = JsonObject("name" -> s"$host.$domain", "type" -> "A") // PowerDNS requires that name be FQDN, not just hostname
      val data = HttpData(Compact(req), charset = HttpCharsets.`UTF-8`)
      val putRequest = (path: Uri) => HttpRequest(method = HttpMethods.PUT,
        uri = path,
        entity = HttpEntity(MediaTypes.`application/json`, data))

      pipelineRequest(Uri.Path(s"$apiRoot/$domain"), putRequest, getHostDNSRecord)
    }

    get(host, domain).flatMap {
      case Some(record) => Future.successful( Some(record) ) // create() should be idempotent: return existing record if there is one
      case None         => createNewRecord(host, domain)
    }
  }

  // curl -u platform-api -X PUT -H 'Content-Type: application/json' -d '{"name": "platform-test.dev.pages", "type": "A", }' http://pdns-master0.util.pages/v1/dev.pages
  //   {"status": "created", "host": {"content": "10.8.0.16", "id": 69119, "type": "A", "name": "platform-test.dev.pages", "ttl": 3600}}
  def createWithKnownAddress(host: String, domain: String, address: String): Future[Option[DNSRecord]] = {
    val req = JsonObject("name" -> s"$host.$domain", "type" -> "A", "content" -> address) // PowerDNS requires that name be FQDN, not just hostname
    val data = HttpData(Compact(req), charset = HttpCharsets.`UTF-8`)
    val putRequest = (path: Uri) => HttpRequest(method = HttpMethods.PUT,
      uri = path,
      entity = HttpEntity(MediaTypes.`application/json`, data))

    pipelineRequest(Uri.Path(s"$apiRoot/$domain"), putRequest, getHostDNSRecord)
  }

  // If an A record exists for $host in $domain, delete it.

  // curl -u platform-api -X DELETE -H 'Content-Type: application/json' http://pdns-master0.util.pages/v1/record/69119
  //   {"status": "deleted", "hosts": {"content": "10.8.0.16", "id": 69119, "type": "A", "name": "platform-test.dev.pages", "ttl": 3600}}
  def delete(host: String, domain: String): Future[Unit] = {
    def deleteExistingRecord(recordId: Int): Future[Unit] = {
      val deleteRequest = (path: Uri) => HttpRequest(method = HttpMethods.DELETE, uri = path)
      val ignored = (x: Option[Json]) => (): Unit
      pipelineRequest(Uri.Path(s"$apiRoot/record/$recordId"), deleteRequest, ignored)
    }

    get(host, domain).flatMap {
      case None         => Future.successful( (): Unit )
      case Some(record) => deleteExistingRecord(record.id)
    }
  }

}
