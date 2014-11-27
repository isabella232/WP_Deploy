package com.whitepages.sampleApp

import com.codahale.metrics.MetricRegistry
import com.whitepages.framework.service.JsonService

class SampleMain extends JsonService {
  val serviceName = "sample-app"
  val handlerFactory = SampleHandlerFactory
  val monitorExt:Option[(MetricRegistry) => PartialFunction[Any, Unit]] = None
  val queryStringHandler = None
}

object SampleMain {

  def main(args: Array[String]) {
    val sample = new SampleMain()
    sample.runServer()
  }

}
