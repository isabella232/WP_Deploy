package com.whitepages.serviceAgent

import akka.actor.ActorSelection
import com.persist.JsonOps._
import com.typesafe.config.Config
import com.whitepages.framework.logging.noId
import com.whitepages.framework.util.ClassSupport
import com.whitepages.platformCommon.credentials.SecureCredentialProvider
import com.whitepages.platformCommon.loadBalancer.LoadBalancerConfig
import com.whitepages.platformCommon.loadBalancer.balancer.{Node, ZeusControl}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps


/*
Currently this class captures information about the service that is required by the load balancer:
- service name
- environment
- service IP address
- service port

 */
case class LoadBalancer(svcName: String, balancerScope: String, nodeIpAddress: String, nodePort: Int, balancerConfig: Config)(implicit val ec: ExecutionContext) extends ClassSupport with ZeusControl with LoadBalancerConfig {

  val zeusConfig = ZeusControl.extractConfig(balancerConfig, balancerScope, SecureCredentialProvider) // default credential provider

  ping()

  def ping(): Unit = {
    isHealthy().onSuccess { case isHealthy =>
      log.info(noId, s"${if (isHealthy) "Can" else "Cannot"} talk to '$balancerScope' load balancer ${zeusConfig.controlAddress}:${zeusConfig.controlPort} for $svcName($nodeIpAddress:$nodePort)")}
  }

  def enableNode(listenerOpt: Option[ActorSelection], message: String) {
    val servicePool = nameFor(svcName, "pool")
    enableNodes(servicePool, Seq(Node(nodeIpAddress, nodePort, None)))
      .map(json => {
        log.info(noId, JsonObject("lbEnableResponse" -> json))
        listenerOpt.foreach(_ ! message)
      })
  }

  def disableNode(listenerOpt: Option[ActorSelection], message: String) {
    val servicePool = nameFor(svcName, "pool")
    disableNodes(servicePool, Seq(Node(nodeIpAddress, nodePort, None)))
      .map(json => {
        log.info(noId, JsonObject("lbDisableResponse" -> json))
        listenerOpt.foreach(_ ! message)
      })
  }

  def queryStatus(): Future[Json] = {
    val target = s"$nodeIpAddress:$nodePort"

    def nodesStatusToJson(ns: Seq[Node]): Json =
      JsonObject(target -> (ns.find(n => n.host == nodeIpAddress && n.port == nodePort) match {
        case Some(n) => if (n.isEnabled) "enabled" else "disabled"
        case None => "unknown"
      }))

    query(nameFor(svcName, "pool"))
      .map(nodesStatusToJson)
      .recover { case t: Throwable =>
        log.error(noId, JsonObject("msg" -> "Exception while querying load balancer"), t)
        JsonObject(s"" -> "unknown")
      }
  }
}
