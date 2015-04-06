package com.whitepages.platformController.main

import com.codahale.metrics.MetricRegistry
import com.whitepages.framework.service.JsonService

class PlatformControllerMain extends JsonService {
  val serviceName = "platform-controller"
  val handlerFactory = new PlatformControllerHandlerFactory(true)
  val monitorExt:Option[(MetricRegistry) => PartialFunction[Any, Unit]] = None
  val queryStringHandler = None
}

object PlatformControllerMain {

  def main(args: Array[String]) {
    val pc = new PlatformControllerMain()
    pc.runServer()
  }

}
