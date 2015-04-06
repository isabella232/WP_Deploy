package com.whitepages.serviceAgent

import com.codahale.metrics.MetricRegistry
import com.whitepages.framework.service.JsonService

class AgentMain extends JsonService {
  val serviceName = "service-agent"
  val handlerFactory = new AgentHandlerFactory(this)
  val monitorExt:Option[(MetricRegistry)=>PartialFunction[Any,Unit]] = None
  val queryStringHandler = None
}

object AgentMain {

  def main(args: Array[String]) {
    val agent = new AgentMain()
    agent.runServer(readCommands = false)
  }

}
