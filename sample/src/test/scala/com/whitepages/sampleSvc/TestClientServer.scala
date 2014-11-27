package com.whitepages.sampleSvc

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import com.typesafe.config.ConfigFactory
import com.persist.JsonOps._
import com.whitepages.framework.logging.{RequestId, noId}
import com.whitepages.framework.service.ClientCallback
import scala.language.postfixOps
import com.whitepages.framework.util.ClassSupport
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import com.whitepages.framework.client.Client

class TestClientServer extends FunSpec with BeforeAndAfterAll
  with Matchers with ClassSupport {
  private[this] val main = new SampleMain()
  private[this] var client:Client = null
  private[this] var info:ClientCallback.Info = null

  override def beforeAll {
    val testConfig = Some(ConfigFactory.parseResources("sample-svc-test.conf"))
    info = main.testBeforeSystem(testConfig)
    client = Client(info.refFactory, "self")
  }

  override def afterAll {
    Await.result(client.stop,30 seconds)
    main.testAfter()
  }

  describe("valid JSON requests") {

    it("should respond correctly") {
      val request = JsonObject(
        "address1" -> "1301 5th ave",
        "address2" -> "1600",
        "location" -> "seattle, WA")
      val request1 = JsonObject("request" -> request)
      val f = client.postJson("testcmd", request1, RequestId())
      val response = Await.result(f, 10 seconds)
      jget(response, "success", "zip") should equal("2625")
      jget(response, "success", "county") should equal("King")
      log.info(noId, response)
    }
  }

}
