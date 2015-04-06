package com.whitepages.platformController.provision

import akka.actor.ActorRefFactory
import com.whitepages.framework.util.ClassSupport
import com.whitepages.platformCommon.credentials.SecureCredentialProvider
import com.whitepages.platformCommon.jclouds.InstanceRequest
import com.whitepages.platformCommon.orchestration.Orchestrator
import com.whitepages.platformCommon.orchestration.Orchestrator.{PromisedType, BuildService}
import scala.concurrent.duration._

import scala.concurrent.{Await, Promise}

/*
  DESIGN GUIDELINES

    Provisioning details should be hidden inside
    Versioning of CoreOs and standard containers should be inside
    Getting and checking parameters should be outside
    Locking and all etcd access should be outside

    Today this API should support both AWS and Openstack
    In the future it should also support Kubernetes
 */

object Provisioner {
  private[provision] case class ZoneDetails(region: String, az: String, domain: String)

  private[provision] def getZoneDetails(zone: String, provider: String): ZoneDetails = {
    (zone, provider) match {
      case ("util",    "openstack") => ZoneDetails("us-seattle", "us-seattle-prod", "util.pages")
      case ("prod",    "openstack") => ZoneDetails("us-seattle", "us-seattle-prod", "prod.pages")
      case ("staging", "openstack") => ZoneDetails("us-seattle", "us-seattle-prod",  "stg.pages")
      case ("qa",      "openstack") => ZoneDetails("us-seattle", "us-seattle-dev",    "qa.pages")
      case (_,         "aws")       => ZoneDetails("us-west-2",  "us-west-2a",       "aws.pages")
      case default => throw new IllegalArgumentException(s"Unknown zone/provider ($zone, $provider)")
    }
  }
}

case class Provisioner(factory: ActorRefFactory) extends ClassSupport {
  import Provisioner._

  def addInstance() = {
    // TODO this should set up the load balancer
  }

  def addSub(service: String, environment: String, sub: String, zone: String, port: Int,
             provider: String, instanceType: String, count: Int,
             weight: Int, lb:String, select:Seq[String]): PromisedType = {
    // TODO use weight
    // TODO use sub

    val ZoneDetails(region, az, domain) = getZoneDetails(zone, provider)

    val instanceRequest = InstanceRequest(provider, region, az, environment, service, instanceType, domain, count,
      config.getString("wp.platform-controller.agentVersionInCloudConfig"))
    val orchestrator = factory.actorOf(Orchestrator.getProps(SecureCredentialProvider))
    val p = Promise[Orchestrator.PromisedType]()
    orchestrator ! BuildService(service, port, instanceRequest, p, imageNameOpt = None, lbType = lb)
    Await.result(p.future, 4.minutes)
  }

  def changeCount() = ???

  def changeWeight() = ???

  def upgradeSub() = {
    // TODO do a rolling reprovision to update coreos and container versions
    ???
  }

  def deleteInstance() = ???

  def deleteSub() = ???

}
