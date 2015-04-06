package com.whitepages.platformCommon.jclouds

import java.util.{NoSuchElementException, Properties}

import com.google.common.collect.ImmutableSet
import com.whitepages.platformCommon.credentials.CredentialProvider
import com.whitepages.platformCommon.crypto.Credentials
import com.whitepages.platformCommon.loadBalancer.balancer.ZeusConfig
import org.jclouds.aws.ec2.compute.AWSEC2TemplateOptions
import org.jclouds.compute.domain.{NodeMetadata, Template}
import org.jclouds.compute.{ComputeServiceContext, RunNodesException}
import org.jclouds.logging.slf4j.config.SLF4JLoggingModule
import org.jclouds.openstack.nova.v2_0.compute.options.NovaTemplateOptions
import org.jclouds.sshj.config.SshjSshClientModule
import org.jclouds.{Constants, ContextBuilder}

import scala.collection.immutable.HashSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source // TODO: replace

case class ProvisioningResult(nodes: Set[NodeMetadata], errorOpt: Option[Throwable])

case class InstanceRequest(provider: String,
                           region: String,
                           zone: String,
                           env: String,
                           owner: String,
                           instanceType: String,
                           domain: String,
                           count: Int,
                           serviceAgentVersion: String
                           )

object ProvisioningControl {
  def getProvider(name: String, credsProvider: CredentialProvider): Option[ProvisioningControl] = name match {
    case "aws" => Some(new AWSProvisioningControl {
      val credentialProvider = credsProvider
      val defaultImage = "ami-bd2d728d"
    })

    case "openstack" => Some(new OpenstackProvisioningControl {
      val credentialProvider = credsProvider
      val defaultImage = "coreos-557.1.0"
      val index = OpenstackResourceIndex.basic
    })

    case _ => None
  }

  // TODO: refactor
  def getCloudConfigBytes(r: InstanceRequest, zeusConfigOpt: Option[ZeusConfig]): Option[Array[Byte]] =
    Option(getClass.getResource("/cloud_config.yml")) map {
      case url =>
        val (primaryResolver, secondaryResolver) = resolvers(r.domain)
        val text = Source
          .fromURL(url)
          .getLines()
          .filter(l => // fragile code, will break on boundaries
            if (zeusConfigOpt.isDefined) true
            else                         !l.contains("[LB-")
          )
          .mkString("\n")
        val intermediate = text.replaceAll( """\[SERVICE\-AGENT\-VERSION\]""", r.serviceAgentVersion)
          .replaceAll( """\[DNS\-RESOLVER\-PRIMARY\]""", primaryResolver)
          .replaceAll( """\[DNS\-RESOLVER\-SECONDARY\]""", secondaryResolver)
          .replaceAll( """\[DNS\-DEFAULT\-DOMAIN\]""", r.domain)
          .replaceAll( """\[PRIVATE\-KEY\]""", Credentials.getPrivate())
        val result =
          zeusConfigOpt match {
            case Some(zc) =>
              intermediate
                .replaceAll( """\[LB\-ENV\]""", r.domain.split('.').head.toUpperCase)
                .replaceAll( """\[LB\-USER\]""", zc.controlUser)
                .replaceAll( """\[LB\-PASSWORD\]""", zc.controlPassword)
            case None => intermediate
          }
        result.getBytes("UTF-8") // why doesn't BuildInfo include the correct version?
    }

  val resolvers = Map(
    "dev.pages"  -> ("10.8.101.50", "10.8.101.51"),
    "qa.pages"   -> ("10.16.0.221", "10.16.0.222"),
    "stg.pages"  -> ("10.80.0.148", "10.80.0.149"), // Doesn't have its own -- uses util
    "aws.pages"  -> ("10.80.0.148", "10.80.0.149"), // Doesn't have its own -- uses util
    "prod.pages" -> ("172.16.4.73", "172.16.4.74"),
    "util.pages" -> ("10.80.0.148", "10.80.0.149")
  )

}

trait ProvisioningSettings {
  def subnet: String
  def image: String
}

sealed trait ProvisioningControl {

  protected val group = "platform"

  protected def getBuilder(providerOrApi: String): Option[ContextBuilder] =
    scala.util.control.Exception.catching(classOf[NoSuchElementException]).opt {
      ContextBuilder
        .newBuilder(providerOrApi)
        .modules(ImmutableSet.of(new SLF4JLoggingModule(), new SshjSshClientModule()))
    }

  protected def buildTemplate(context: ComputeServiceContext, locationId: String, imageId: String, hardwareId: String): Template =
    context.getComputeService.templateBuilder()
      .locationId(locationId)
      .imageId(imageId)
      .hardwareId(hardwareId)
      .build()

  protected def asyncCreatedNodes(group: String, count: Int, template: Template, context: ComputeServiceContext): Future[ProvisioningResult] = {

    def convertJnodes(jnodes: java.util.Set[_ <: NodeMetadata]): HashSet[NodeMetadata] = {
      import scala.collection.JavaConverters._
      jnodes.asScala.to[({type s[_] = collection.immutable.HashSet[NodeMetadata]})#s]
    }

    val resultF = Future {
      val allNodes = convertJnodes(context.getComputeService.createNodesInGroup(group, count, template))
      ProvisioningResult(allNodes, None)
    }.recover {
      case rne: RunNodesException =>
        val successfulNodes = convertJnodes(rne.getSuccessfulNodes)
        ProvisioningResult(successfulNodes, Some(rne))
      case other: Throwable =>
        ProvisioningResult(Set.empty[NodeMetadata], Some(other))
    }
    resultF.onComplete { case _ => context.close()}
    resultF
  }

  def create(r: InstanceRequest, zeusConfigOpt: Option[ZeusConfig]): Future[ProvisioningResult]
  def credentialProvider: CredentialProvider
  def settings: ProvisioningSettings
}

trait AWSProvisioningControl extends ProvisioningControl {

  val settings = new ProvisioningSettings {
    val subnet = "subnet-dfe6ceab"
    val image  = "ami-bd2d728d"
  }

  // instanceType is a selection from a hardcoded list:
  // https://github.com/jclouds/jclouds/blob/d3c1e2eab4bd71ecdf2ebfcd73e5d28c835398cd/providers/aws-ec2/src/main/java/org/jclouds/aws/ec2/compute/suppliers/AWSEC2HardwareSupplier.java
  def create(r: InstanceRequest, zeusConfigOpt: Option[ZeusConfig]): Future[ProvisioningResult] = {
    getBuilder("aws-ec2") match {
      case None => throw new IllegalArgumentException("missing AWS builder")
      case Some(builder) =>
        val credentials = credentialProvider.awsCredentials()
        val context = builder
          .credentials(credentials.identity, credentials.secret)
          .buildView(classOf[ComputeServiceContext])

        val template = buildTemplate(context, r.zone, s"${r.region}/${settings.image}", r.instanceType)

        val tmplOpts = template.getOptions.as(classOf[AWSEC2TemplateOptions])
          .keyPair("platform")
          .subnetId("subnet-dfe6ceab")
          .userMetadata("Owner", r.owner)
          .userMetadata("ENV", r.env)

        // nameserver [DNS-RESOLVER-PRIMARY]
        // nameserver [DNS-RESOLVER-SECONDARY]
        // search [DNS-DEFAULT-DOMAIN]

        ProvisioningControl.getCloudConfigBytes(r, zeusConfigOpt).foreach(data => tmplOpts.userData(data))

        asyncCreatedNodes(group, r.count, template, context)
    }
  }
}

trait OpenstackResourceIndex {
  def instanceTypes: Map[String, Map[String, String]]
  def images: Map[String, Map[String, String]]
  def networks: Map[String, String]
  def defaultNetwork: String

  def instanceType(location: String, name: String): String = idForLocation(instanceTypes, location, name)

  def image(location: String, name: String): String = idForLocation(images, location, name)

  // Networks are, so far, not region-specific in Openstack
  def network(name: String): String = {
    networks.getOrElse(name, defaultNetwork)
  }

  private def idForLocation(data: Map[String, Map[String, String]], location: String, key: String) = {
    s"$location/${data.get(location).get(key)}"
  }
}

// FIXME replace this with dynamic discovery
object OpenstackResourceIndex {

  def basic = new OpenstackResourceIndex {
    val instanceTypes = Map("us-seattle" -> Map(
      "m1.small"     -> "2",
      "m1.medium"    -> "3",
      "m1.large"     -> "4",
      "m1.xlarge"    -> "5",
      "m3.medium"    -> "3dec2f84-bfe3-44ed-88a7-89a7b1793d9b",
      "m3.large"     -> "74abedc7-6537-4ff1-abe0-81afa34d857c",
      "m3.xlarge"    -> "a8146fab-8166-423b-8d13-b1850d41ffc2",
      "m3.2xlarge"   -> "5dc66a4d-34a0-4112-957e-1aede36fe43f",
      "c3.large"     -> "f8122478-1e36-40b2-8631-7247820e1885",
      "c3.xlarge"    -> "160effeb-199b-4bca-8d6a-9e64215a8b3b",
      "c3.2xlarge"   -> "6bb0a4d3-fb59-4889-838a-afd453b0ff4c",
      "c3.4xlarge"   -> "43b06638-11ca-439e-afe9-e650b3209e35",
      "c3.8xlarge"   -> "36122535-db50-45cf-9c01-81083e03c14f",
      "wp4.60xlarge" -> "1ea2da86-8b8c-42ed-846a-e326b7f3e37f"
    ))

    val images = Map("us-seattle" -> Map(
      "coreos-557.1.0"                          -> "5924c78d-e03c-4953-a7de-297fc1c8ed1b",
      "coreos-beta-20141216"                    -> "2e1aa8ea-6fee-4a8f-a29e-f69d1fe58e8a",
      "coreos-alpha-20141216"                   -> "bd1fa41c-8ffb-4b16-8c21-8e3a02dcda8b",
      "lucid-server-cloudimg-amd64-20150227.wp" -> "3089a504-b9ea-4c77-91b7-e30e949ec196",
      "precise-server-cloudimg-amd64-20141212"  -> "f6f12489-81cc-4d8b-90ca-19af7e0f4dff",
      "trusty-server-cloudimg-amd64-20141212"   -> "9cd26e5d-b276-4354-8596-868bf590d58a",
      "utopic-server-cloudimg-amd64-current"    -> "068f8256-6082-4374-bea9-3793b31f79f8"
    ))

    // Networks are, so far, not region-specific in Openstack
    val networks = Map(
      "dev.pages"  -> "424e2cf8-2f0c-4ded-a139-05127caf24f4",
      "qa.pages"   -> "76953ac5-4975-4930-86bb-8d913d3152ab",
      "stg.pages"  -> "e8cc861f-22f3-4197-850d-ffa9772d80aa",
      "prod.pages" -> "c29f6363-63a1-4c13-a595-b44c4a42a3c8",
      "util.pages" -> "504cfd96-52a2-4f72-ba32-922ee507c057"
    )


    val defaultNetwork = "424e2cf8-2f0c-4ded-a139-05127caf24f4" // dev.pages
  }
}

object OpenstackProvisioningControl {
  case class OpenstackConfig(endpoint: String, project: String)

  def resolveUrlAndProject: OpenstackConfig = {
    val envKeys = Seq("OS_AUTH_URL", "OS_PROJECT_NAME")
    val values = envKeys.map(e => Option(System.getenv(e))).collect { case Some(s) => s}
    if (values.size < envKeys.size)
      throw new IllegalArgumentException(s"OpenStack environment variables not configured; ensure that ${envKeys.mkString(", ")} are all appropriately set")
    OpenstackConfig(values(0), values(1))
  }
}

trait OpenstackProvisioningControl extends ProvisioningControl {

  val settings = new ProvisioningSettings {
    val subnet = "dev.pages"
    val image  = "coreos-557.1.0"
  }

  // val index = OpenstackResourceIndex.basic
  def index: OpenstackResourceIndex

  def create(r: InstanceRequest, zeusConfigOpt: Option[ZeusConfig]): Future[ProvisioningResult] = {
    val overrides = new Properties() {
      setProperty(Constants.PROPERTY_TRUST_ALL_CERTS, "true")
    }

    getBuilder("openstack-nova") match {
      case Some(builder) =>
        val credentials = credentialProvider.openstackCredentials()
        val openstackConfig = OpenstackProvisioningControl.resolveUrlAndProject
        val context =
          builder
          .endpoint(openstackConfig.endpoint)
          .credentials(s"${openstackConfig.project}:${credentials.identity}", credentials.secret)
          .overrides(overrides)
          .buildView(classOf[ComputeServiceContext])

        val template = buildTemplate(context, r.region, index.image(r.region, settings.image), index.instanceType(r.region, r.instanceType))

        val tmplOpts = template.getOptions.as(classOf[NovaTemplateOptions])
          .keyPairName("platform-well-known")
          .availabilityZone(r.zone)
          .networks(index.network(r.domain))
          .userMetadata("Owner", r.owner)
          .userMetadata("ENV", r.env)

        ProvisioningControl.getCloudConfigBytes(r, zeusConfigOpt).foreach(data => tmplOpts.userData(data))

        asyncCreatedNodes(group, r.count, template, context)
      case None => throw new IllegalArgumentException("missing AWS builder")
    }
  }
}
