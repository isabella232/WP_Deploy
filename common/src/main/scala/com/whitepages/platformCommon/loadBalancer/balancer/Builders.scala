package com.whitepages.platformCommon.loadBalancer.balancer

import java.net.InetAddress

import com.persist.JsonMapper._
import com.persist.JsonOps._

import scala.concurrent.Future

abstract sealed class AbstractBuilder(control: ZeusControl, name: String, json: Json) {
  def build(): Future[Json]
}


case class PoolBuilder(control: ZeusControl, name: String, json: Json) extends AbstractBuilder(control, name, json) {

  def withNodes(nodes: Seq[Node]): PoolBuilder = {
    val zNodes = nodes.map(n => ZeusDomainModel.Node( node = n.toString
      , state = if (n.isEnabled) ZeusDomainModel.Active.value else ZeusDomainModel.Disabled.value
    ))
    PoolBuilder(control, name, jput(json, "properties", "basic", "nodes_table")(emptyJsonArray ++ zNodes.map(zn => ToJson(zn))))
  }

  def build(): Future[Json] =
    control.doCreatePool(name, json)
}


case class VipBuilder(control: ZeusControl, name: String, json: Json) extends AbstractBuilder(control, name, json) {

  def withPort(port: Int): VipBuilder =
    VipBuilder(control, name, jput(json, "properties", "basic", "port")(port))

  def withPool(poolName: String): VipBuilder =
    VipBuilder(control, name, jput(json, "properties", "basic", "pool")(poolName))

  /*
  By default the Vip is disabled, and requires enabling it explicitly. This behavior can be changed by modifying
  the .json template
   */
  def withEnabled(isEnabled: Boolean): VipBuilder =
    VipBuilder(control, name, jput(json, "properties", "basic", "enabled")(isEnabled))

  /*
            "listen_on_any": false,
            "listen_on_hosts": [],
            "listen_on_traffic_ips": [
                "tf-qa65-mcg-vip0",
                "tf-qa65-msw-vip0"
            ],
   */
  def withListenerOnTrafficIps(trafficIpGroups: Seq[String]): VipBuilder = {
    val jsonOnAnyFalse = jput(json, "properties", "basic", "listen_on_any")(false)

    VipBuilder(control, name, jput(jsonOnAnyFalse, "properties", "basic", "listen_on_traffic_ips")(emptyJsonArray ++ trafficIpGroups))
  }

  override def build(): Future[Json] = {
    control.doCreateVip(name, json)
  }
}


case class TrafficIpGroupBuilder(control: ZeusControl, name: String, json: Json) extends AbstractBuilder(control, name, json) {

  def withIpAddresses(ips: Seq[InetAddress]): TrafficIpGroupBuilder =
    TrafficIpGroupBuilder(control, name, jput(json, "properties", "basic", "ipaddresses")(emptyJsonArray ++ ips.map(_.getHostAddress)))

  def withMachines(names: Seq[String]): TrafficIpGroupBuilder =
    TrafficIpGroupBuilder(control, name, jput(json, "properties", "basic", "machines")(emptyJsonArray ++ names))

  def build(): Future[Json] =
    control.doCreateTrafficIpGroup(name, json)
}
