package com.whitepages.sampleApp

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
        "first" -> "John",
        "last" -> "Smith")
      val f = client.postJson("testcmd", request, RequestId())
      val response = Await.result(f, 10 seconds)
      jget(response, "in", "last") should equal("Smith")
      jget(response, "out") should equal("ok")
      log.info(noId, response)
    }
  }

}
