package com.whitepages.sampleSvc

import com.whitepages.framework.loadTest.Run
import com.typesafe.config.ConfigFactory

object LoadTest {

  def main(args:Array[String]) {
    val testConfig = Some(ConfigFactory.parseResources("sample-svc-load.conf"))
    val m = new SampleMain()
    Run.run(m, args, testConfig)
  }

}
