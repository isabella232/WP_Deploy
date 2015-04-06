package com.whitepages.platformCommon.orchestration

import java.net.InetAddress

import akka.actor._
import akka.pattern.pipe
import com.typesafe.config.Config
import com.whitepages.framework.logging.noId
import com.whitepages.framework.util.ActorSupport
import com.whitepages.platformCommon.credentials.CredentialProvider
import com.whitepages.platformCommon.dns.DNSControl
import com.whitepages.platformCommon.jclouds.{InstanceRequest, ProvisioningControl, ProvisioningResult}
import com.whitepages.platformCommon.loadBalancer.LoadBalancerConfig
import com.whitepages.platformCommon.loadBalancer.balancer.{LoadBalancerProviderException, Node, ZeusConfig, ZeusControl}
import com.whitepages.platformCommon.orchestration.Orchestrator.Complete
import com.whitepages.platformCommon.orchestration.Worker._
import org.jclouds.compute.RunNodesException
import org.jclouds.compute.domain.NodeMetadata

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.runtime.universe._


object Worker {
  val maxRetries = 3

  sealed trait WorkerMessage
  case class ProvisionServiceInfrastructure(provisioner: ProvisioningControl, computeFabricRequest: InstanceRequest, lbType: String) extends WorkerMessage
  case class ProvisionNodes(provisioner: ProvisioningControl, computeFabricRequest: InstanceRequest, partialResult: Set[NodeMetadata] = Set.empty[NodeMetadata], retry: Int=0) extends WorkerMessage
  case class ProvisionDns(domain: String, retry: Int=0) extends WorkerMessage {
    def inc: WorkerMessage =
      if (retry < maxRetries) this.copy(retry = this.retry + 1)
      else AbortWork
  }
  case class CreatePoolAndNodeDns(nodes: Set[NodeMetadata], domain: String) extends WorkerMessage
  case class CreatePool(nodes: Set[String], retry: Int=0) extends WorkerMessage {
    def inc: WorkerMessage =
      if (retry < maxRetries) this.copy(retry = this.retry + 1)
      else AbortWork
  }
  case class ConfigureNodesDns(nodesAndIps: Map[String, String], domain: String) extends WorkerMessage
  case object NodeDnsRecordsCreated extends WorkerMessage
  case object AbortWork extends WorkerMessage
  case class CreateTrafficIpGroup(ip: String) extends WorkerMessage
  case class PoolReady(name: String) extends WorkerMessage
  case class TrafficIpGroupReady(name: String) extends WorkerMessage
  case object ServiceProvisioned extends WorkerMessage

  case class FutureTimeoutException(msg: String) extends RuntimeException(msg)

  def getProps(name: String, port: Int, workerConfig: Config, credentialProvider: CredentialProvider) =
    Props(classOf[Worker], name, port, workerConfig, credentialProvider)
}


case class Worker(name: String, port: Int, workerConfig: Config, credentialProvider: CredentialProvider) extends ActorSupport with DNSControl with WorkerConfigKeys with LoadBalancerConfig {
  // DNSControl
  protected implicit val ec: ExecutionContext = context.dispatcher
  def dnsControlHost: String =
    context.system.settings.config.getString(s"wp.$dnsControlHostKey") // for now pull the DNS host from the global config instead of the worker's


  private def handleFailure(msg: String, retryOpt: Option[WorkerMessage]=None): PartialFunction[Throwable, _ <: WorkerMessage] = {
    case fte: FutureTimeoutException =>
      log.error(noId, s"$msg timeout", fte)
      retryOpt.getOrElse(AbortWork)
    case lbex: LoadBalancerProviderException =>
      log.error(noId, s"$msg load balancer exception: ${lbex.msg}", lbex)
      retryOpt.getOrElse(AbortWork)
  }

  private def withTimeout[T](f: Future[T], t: FiniteDuration)(implicit ev: TypeTag[T]): Future[T] = {
    val p = Promise[T]()
    context.system.scheduler.scheduleOnce(t)(p.failure(FutureTimeoutException("timeout")))
    Future.firstCompletedOf(Seq(f, p.future))
  }


  case class WorkerState( poolNameOpt: Option[String]
                        , trafficIpGroupNameOpt: Option[String]
                        , computeNodes: Set[NodeMetadata]
                        , isNodeDnsConfigured: Boolean
                        , zControlOpt: Option[ZeusControl]=None
                        ) {
    def addNodes(other: Set[NodeMetadata]): WorkerState =
      this.copy(computeNodes = this.computeNodes ++ other)

    def withLoadBalancerControl(zConfig: ZeusConfig): WorkerState = {
      val zeusControl = new ZeusControl {
        def zeusConfig: ZeusConfig = zConfig

        protected implicit def system: ActorSystem = context.system
      }
      this.copy(zControlOpt = Some(zeusControl))
    }

    /*
    For now do nothing
     */
    def withoutLoadBalancer(): WorkerState = this
  }

  def receive = statefulReceive(WorkerState(None, None, HashSet.empty[NodeMetadata], isNodeDnsConfigured = false))

  val futureTimeout = 5.minute // TODO: factor out in config

  def statefulReceive(workerState: WorkerState): Receive = {
    case ProvisionServiceInfrastructure(provisioner, request, lbType) =>
      val balancerScope = request.domain.split('.').head // this always succeeds because of the similar code in the Orchestrator

      val newState =
        if (lbType == "whitepages")
          workerState.withLoadBalancerControl(ZeusControl.extractConfig(workerConfig, balancerScope, credentialProvider))
        else
          workerState.withoutLoadBalancer()
      context.become(statefulReceive(newState))
      self ! ProvisionNodes(provisioner, request)
      self ! ProvisionDns(request.domain)
    case pdm: ProvisionDns =>
      withTimeout(getOrCreate(name, pdm.domain), futureTimeout)
        .map {
          case Some(dnsRecord) =>
            CreateTrafficIpGroup(dnsRecord.content)
          case None => AbortWork
        }
        .recover(handleFailure("dns provisioning", retryOpt = Some(pdm.inc)))
        .pipeTo(self)
    case pnm: ProvisionNodes =>
      val zConfigOpt = workerState.zControlOpt.map(_.zeusConfig)
      withTimeout(pnm.provisioner.create(pnm.computeFabricRequest, zConfigOpt), futureTimeout)
        .map {
          case ProvisioningResult(nodes, None) =>
            CreatePoolAndNodeDns(pnm.partialResult ++ nodes, pnm.computeFabricRequest.domain)
          case ProvisioningResult(nodes, Some(pex: RunNodesException)) if nodes.nonEmpty => // worth retrying if at least one node succeeded
            log.error(noId, s"Node creation exception: ${nodes.size}/${pnm.computeFabricRequest.count} created", pex)
            if (pnm.retry < Worker.maxRetries) {
              val updatedRequest = pnm.computeFabricRequest.copy(count = pnm.computeFabricRequest.count - nodes.size)
              log.info(noId, s"Retrying node creation for ${updatedRequest.count} remaining nodes")
              pnm.copy(computeFabricRequest = updatedRequest, partialResult = pnm.partialResult ++ nodes, retry = pnm.retry + 1)
            } else {
              log.error(noId, s"Node creation exception: ${nodes.size}/${pnm.computeFabricRequest.count} created; no more retries left", pex)
              AbortWork
            }
          case ProvisioningResult(nodes, Some(ex)) => // no retries
            log.error(noId, s"Instance provisioning failed; aborting", ex)
            AbortWork
        }
        .recover(handleFailure("compute node creation"))
        .pipeTo(self)
    case AbortWork =>
      context.stop(self)
    case CreatePoolAndNodeDns(nodes, domain) =>
      val newState = workerState.addNodes(nodes)
      context.become(statefulReceive(newState))
      val nodesAndIps = HashMap(newState.computeNodes.map(node => node.getName -> node.getPrivateAddresses.asScala.head).toSeq: _*)
      self ! CreatePool(nodesAndIps.values.toSet)
      self ! ConfigureNodesDns(nodesAndIps, domain)
    case ConfigureNodesDns(nodesAndIps, domain) =>
      val dnsRecordsF =
        Future.traverse(nodesAndIps.toSeq) { case (nodeName, ip) => createWithKnownAddress(nodeName, domain, ip) }
      withTimeout(dnsRecordsF, futureTimeout)
        .map(dnsRecordsOpt => NodeDnsRecordsCreated)
        .recover(handleFailure("dns node provisioning"))
        .pipeTo(self)
    case cpm: CreatePool =>
      val poolName = nameFor(name, "pool")

      def createPool(z: ZeusControl) = {
        z.createPool(poolName)
          .withNodes(cpm.nodes.toSeq.map(ip => Node(host = ip, port = port, isEnabledOpt = Some(false))))
          .build()
          .map(json => PoolReady(poolName))
          .recover(handleFailure("pool creation", retryOpt = Some(cpm.inc)))
          .pipeTo(self)
      }

      workerState.zControlOpt match {
        case Some(zeusControl) => createPool(zeusControl)
        case None => updateState(workerState.copy(poolNameOpt = Some("none")))
      }
    case CreateTrafficIpGroup(ip) =>
      val trafficIpGroup = nameFor(name, "traffic-ip")

      def createTrafficIpGroup(z: ZeusControl) = {
        z.createTrafficIpGroup(trafficIpGroup)
          .withIpAddresses(Seq(InetAddress.getByName(ip)))
          .withMachines(workerConfig.getStringList(lbControlTrafficManagersKey).asScala)
          .build()
          .map(json => TrafficIpGroupReady(trafficIpGroup))
          .recover(handleFailure("traffic IP group creation"))
          .pipeTo(self)
      }

      workerState.zControlOpt match {
        case Some(zeusControl) => createTrafficIpGroup(zeusControl)
        case None => updateState(workerState.copy(trafficIpGroupNameOpt = Some("none")))
      }
    case NodeDnsRecordsCreated =>
      log.info(noId, "Compute nodes DNS configured; updating state")
      val newState = workerState.copy(isNodeDnsConfigured = true)
      updateState(newState)
    case PoolReady(poolName) =>
      log.info(noId, s"Load balancer pool '$poolName' configured; updating state")
      val newState = workerState.copy(poolNameOpt = Some(poolName))
      updateState(newState)
    case TrafficIpGroupReady(trafficIpGroupName) =>
      log.info(noId, s"Load balancer traffic IP group '$trafficIpGroupName' created; updating state")
      val newState = workerState.copy(trafficIpGroupNameOpt = Some(trafficIpGroupName))
      updateState(newState)
    case ServiceProvisioned =>
      log.info(noId, s"Provisioning compute fabric & DNS finished: ${workerState.computeNodes.map(n => n.getName -> n.getPrivateAddresses.asScala).toSeq.mkString(", ")}")
      val ips =
        workerState.computeNodes
          .flatMap(nm => nm.getPrivateAddresses.asScala)
          .to[({type s[_] = collection.immutable.Seq[String]})#s]
      context.parent ! Complete(self, ips)
  }

  def updateState(workerState: WorkerState) = {
    context.become(statefulReceive(workerState), discardOld = true)
    (workerState.poolNameOpt, workerState.trafficIpGroupNameOpt) match {
      case (Some(poolName), Some(trafficIpGroupName)) =>
        val vipName = nameFor(name, "vip")

        def createVip(z: ZeusControl) = {
          z.createVip(vipName)
           .withPort(port)
           .withPool(poolName)
           .withListenerOnTrafficIps(Seq(trafficIpGroupName))
           .withEnabled(isEnabled = true)
           .build()
           .map(json => {
            ServiceProvisioned
            })
           .recover(handleFailure("vip creation"))
           .pipeTo(self)
        }

        workerState.zControlOpt match {
          case Some(zeusControl) => createVip(zeusControl)
          case None => self ! ServiceProvisioned
        }
      case _ => log.info(noId, s"Waiting on VIP creation: state is (${workerState.poolNameOpt}, ${workerState.trafficIpGroupNameOpt})")
    }
  }
}


trait WorkerConfigKeys {
  val dnsControlHostKey = "dnsControl"
  // keys within the balancer scope
  val lbControlTrafficManagersKey = "trafficManagers"
}

