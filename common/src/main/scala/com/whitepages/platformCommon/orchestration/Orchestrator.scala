package com.whitepages.platformCommon.orchestration

import akka.actor._
import com.typesafe.config.{ConfigFactory, ConfigException}
import com.whitepages.framework.logging.noId
import com.whitepages.framework.util.ActorSupport
import com.whitepages.platformCommon.credentials.{CredentialProvider, EnvironmentCredentialProvider}
import com.whitepages.platformCommon.jclouds.{InstanceRequest, ProvisioningControl}
import com.whitepages.platformCommon.orchestration.Orchestrator.{BuildService, Complete, PromisedType}
import com.whitepages.platformCommon.orchestration.Worker.ProvisionServiceInfrastructure

import scala.collection.immutable.HashMap
import scala.concurrent.Promise


object Orchestrator {
  type PromisedType = Seq[String]

  sealed trait OrchestratorMessage
  case class BuildService(serviceName: String, port: Int, computeFabricRequest: InstanceRequest, p: Promise[PromisedType], imageNameOpt: Option[String]=None, lbType: String="whitepages") extends OrchestratorMessage
  case class Complete(worker: ActorRef, ips: Seq[String]) extends OrchestratorMessage

  def getProps(credentialProvider: CredentialProvider=EnvironmentCredentialProvider) =
    Props(classOf[Orchestrator], credentialProvider)

}


class Orchestrator(credentialProvider: CredentialProvider) extends ActorSupport {

  def receive: Receive = receiveWithState(HashMap.empty[ActorRef, Promise[PromisedType]])

  def receiveWithState(inFlight: HashMap[ActorRef, Promise[PromisedType]]): Receive =  {

    case BuildService(name, port, request, p, imageNameOpt, lbType) =>
      validationErrors(port) match {
        case Some(ex) => p.failure(ex)
        case None =>
          val provider = request.provider
          val balancerScope = request.domain.split('.').head
          ProvisioningControl.getProvider(provider, credentialProvider) match {
            case Some(provisioner) =>
              scala.util.control.Exception.catching(classOf[InvalidActorNameException], classOf[ConfigException.Missing], classOf[ConfigException.BadPath])
                .withApply {
                case _: InvalidActorNameException=> p.failure(new IllegalArgumentException(s"request for $name in flight"))
                case _: ConfigException.Missing => p.failure(new IllegalArgumentException(s"no load balancer config for environment '${request.env}'"))
              } {
                val workerConfig =
                  if (lbType == "none") ConfigFactory.empty() // TODO: fix, this is not right
                  else context.system.settings.config.getConfig(s"wp.balancer.$balancerScope")
                val worker = context.actorOf(Worker.getProps(name, port, workerConfig, credentialProvider), s"worker-$name")
                context.watch(worker)
                context.become(receiveWithState(inFlight + (worker -> p)), discardOld = true)
                worker ! ProvisionServiceInfrastructure(provisioner, request, lbType)
              }
            case None =>
              p.failure(new IllegalArgumentException(s"No such compute fabric provider $provider"))
          }
      }
    case Complete(a, ips) =>
      val p = inFlight(a)
      p.success(ips)
      context.become(receiveWithState(inFlight - a), discardOld = true)
      context.unwatch(a)
      context.stop(a)
    case Terminated(a) =>
      inFlight.get(a) match {
        case Some(p) =>
          p.failure(new IllegalArgumentException("provisioning request failed"))
          context.become(receiveWithState(inFlight - a), discardOld = true)
        case None => log.warn(noId, "worker not registered")
      }
  }

  private def validationErrors(port: Int): Option[Throwable] = {

    // TODO: also validate the IP/name
    if (port == 0) {
      log.error(noId, "node port 0 (not set?)")
      Some(new IllegalArgumentException("node port cannot be 0"))
    } else None
  }
}


