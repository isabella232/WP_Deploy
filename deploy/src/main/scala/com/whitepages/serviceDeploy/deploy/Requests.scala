package com.whitepages.serviceDeploy.deploy

import scala.concurrent.Promise
import com.persist.JsonOps._

object Requests {

  case class AgentRequest(cmd: String, options: JsonObject, p: Promise[JsonObject])

  case class AgentTimer(id: Long)

}
