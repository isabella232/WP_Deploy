package com.whitepages.platformController.client

import com.persist.JsonOps._
import com.whitepages.framework.logging.AnyId

import scala.concurrent.Promise

object AgentRequests {

  case class AgentRequest(id:AnyId, cmd: String, options: JsonObject, user:String, p: Promise[JsonObject])

  case class AgentTimer(id: Long)

  case class Progress(host:String, msg:String, level:String = "info")

}
