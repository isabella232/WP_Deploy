package com.whitepages.sampleSvc

import com.codahale.metrics.MetricRegistry
import com.whitepages.framework.service.ThriftService

class SampleMain extends ThriftService {
  val serviceName = "sample-svc"
  val thriftPath = "com.whitepages.generated"
  val thriftName = "Test"
  val handlerFactory = SampleHandlerFactory
  val monitorExt:Option[(MetricRegistry) => PartialFunction[Any, Unit]] = Some(SampleMonitor.ext _)
  val queryStringHandler = None
}

object SampleMain {

  def main(args: Array[String]) {
    val sample = new SampleMain()
    sample.runServer()
  }

}
