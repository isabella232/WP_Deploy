package com.whitepages.sampleSvc

import org.scalatest.{ShouldMatchers, FunSpec}
import com.whitepages.generated.{Request, Response, Test}

class TestActs extends FunSpec with ShouldMatchers {

  it("testcmd should work") {
    val request = Request("1301 5th ave","1600", Some("seattle, WA"))
    val request1 = Test.testcmd$args(request)
    val response = SampleActs.testcmd(request1)
    response.success.get.zip should equal ("2625")
    response.success.get.county should equal ("King")
  }

}
