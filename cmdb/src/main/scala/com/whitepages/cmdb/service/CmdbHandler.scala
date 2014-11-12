package com.whitepages.cmdb.service

import akka.actor.{ActorRefFactory, ActorRef, ActorContext}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import com.persist.JsonOps.Json
import com.whitepages.cmdb.graph.{Neo4jLoad, Neo4jUtil, Neo4jOps}
import com.persist.JsonOps._
import com.whitepages.cmdb.util.Order
import com.whitepages.framework.util.ClassSupport
import scala.language.postfixOps
import com.whitepages.framework.service.JsonService


class CmdbHandler(context: ActorRefFactory, ops: Neo4jOps, util: Neo4jUtil) extends JsonService.Handler with ClassSupport {
  private[this] implicit val timeout = Timeout(10 seconds)
  private[this] implicit val ec = context.dispatcher

  def act(hr: JsonService.Request): Future[JsonService.Response] = {
    val parts = hr.cmd.split("/")

    val f = if (parts.size == 1) {

      (parts(0), hr.request) match {
        case ("service", request: Json) => ops.services
        case ("stack", request: Json) => ops.stacks
        case ("person", request: Json) => ops.people
        case ("load-reload", request: Json) =>
          val load = Neo4jLoad(util)
          for (f1 <- load.clear;
               f2 <- load.nodes;
               f3 <- load.relationships)
          yield JsonObject("clear" -> f1, "nodes" -> f2, "relationships" -> f3)
        //case ("load-nodes", request: Json) => ops.nodes
        //case ("load-relationships", request: Json) => ops.relationships
        //case ("load-clear", request: Json) => ops.clear
        case x: Any =>
          println("Bad command:" + hr.cmd)
          Future {
            "FAIL1:" + hr.cmd
          }
      }
    } else if (parts.size == 2) {
      (parts(0), parts(1), hr.request) match {
        case ("service", name: String, request) => ops.service(name)
        case ("stack", name: String, request) => ops.stack(name)
        case ("person", name: String, request) => ops.person(name)
        case ("cluster", name: String, request) => ops.cluster(name)
        case ("version", name: String, request) => ops.version(name)
        case ("server", name: String, request) => ops.server(name)
        case ("deployment", name: String, request) => ops.deployment(name)
        case x: Any =>
          println("Bad command:" + hr.cmd)
          Future {
            "FAIL2:" + hr.cmd
          }
      }
    } else if (parts.size == 3) {
      (parts(0), parts(1), parts(2), hr.request) match {
        case ("stack", name: String, "deployments", request) => ops.stackDeployments(name)
        case ("service", name: String, "deployments", request) => ops.serviceDeployments(name)
        case ("person", name: String, "deployments", request) => ops.personDeployments(name)
        case x: Any =>
          println("Bad command:" + hr.cmd)
          Future {
            "FAIL3:" + hr.cmd
          }
      }

    } else {
      println("Bad command:" + hr.cmd)
      Future {
        "FAIL:" + hr.cmd
      }
    }

    f map {
      case result => JsonService.Response(result)
    }
  }

}

object CmdbHandlerFactory extends JsonService.HandlerFactory {

  var uiInit: Option[Neo4jOps => Unit] = None

  def start(context: ActorRefFactory) = {
    val util = Neo4jUtil(context)
    val ops = Neo4jOps(util)
    uiInit match {
      case Some(f) => f(ops)
      case None =>
    }
    new CmdbHandler(context, ops, util)
  }

}
