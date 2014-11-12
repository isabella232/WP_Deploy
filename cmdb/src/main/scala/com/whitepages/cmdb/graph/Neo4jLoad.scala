package com.whitepages.cmdb.graph

import com.persist.JsonOps._
import scala.Some
import scala.concurrent.Future
import java.util.UUID
import org.joda.time.DateTime
import com.whitepages.cmdb.util.Order
import com.whitepages.framework.util.{ClassSupport, Util}

case class Neo4jLoad(util: Neo4jUtil) extends ClassSupport {

  import util._

  private[this] implicit val ec = system.dispatcher

  private val ts0 = logFmt.print(new DateTime(2014, 1, 1, 0, 0))
  private val ts1 = logFmt.print(new DateTime(2014, 1, 10, 0, 0))
  private val ts2 = logFmt.print(new DateTime(2014, 1, 20, 0, 0))
  private val ts3 = logFmt.print(new DateTime(2014, 1, 25, 0, 0))

  private var uids = Map[(String, String), String]()

  val jschema = JsonObject(
    "log" -> JsonArray("debug", "info", "warn", "error"),
    "feature1" -> "boolean",
    "name1" -> "string"
  )
  val jdynamic = JsonObject(
    "log" -> "warn",
    "feature1" -> false,
    "name1" -> "foo"
  )

  private def node(name: String, label: String, t0: String = ts0, t1: Option[String] = None, uname: String = "", other: JsonObject = emptyJsonObject) = {
    val id = UUID.randomUUID().toString()
    val n = if (uname != "") uname else name
    uids += ((label, n) -> id)
    val s1 = t1 match {
      case Some(t) => JsonObject("t1" -> t)
      case None => emptyJsonObject
    }
    val props = other ++ s1
    createNode(name, t0, label, props, id = id)
  }

  private def relationship(inName: String, inLabel: String, rel: String, outName: String, outLabel: String) = {
    val rname = inName + "=>" + outName
    val inId = uids((inLabel, inName))
    val outId = uids((outLabel, outName))
    val id = UUID.randomUUID().toString()
    val cmds = Seq(
      "match " + cypherNode(inLabel, "a", props = JsonObject("id" -> inId)),
      "match " + cypherNode(outLabel, "b", props = JsonObject("id" -> outId)),
      "create " + cypherNode(name = "a") + cypherRight(rel, "r", props = JsonObject("id" -> id, "name" -> rname)) + cypherNode(name = "b") + " return r"
    ).mkString(" ")
    cypher(cmds)
  }

  def clear = {
    for (f1 <- cypher("match ()-[r]->() delete r");
         f2 <- cypher("match (n) delete n")
    ) yield JsonObject("r" -> f1, "n" -> f2)
  }

  def nodes = {
    val cmds = Seq(
      node("asc", "Service"),
      node("dirsvc", "Service"),
      node("cass", "Service"),

      node("v1", "Version", uname = "asc-v1"),
      node("v2", "Version", uname = "asc-v2"),
      node("v3", "Version", uname = "asc-v3", other = JsonObject("schema" -> Compact(jschema), "dynamic" -> Compact(jdynamic))),
      node("v1", "Version", uname = "cass-v1"),
      node("v2", "Version", uname = "cass-v2"),
      node("v1", "Version", uname = "dirsvc-v1"),

      node("qa", "Stack"),
      node("prod", "Stack"),

      node("asc", "Cluster", uname = "qa-asc"),
      node("cass", "Cluster", uname = "qa-cass"),
      node("dirsvc", "Cluster", uname = "qa-dirsvc"),
      node("asc", "Cluster", uname = "prod-asc"),
      node("cass", "Cluster", uname = "prod-cass"),
      node("dirsvc", "Cluster", uname = "prod-dirsvc"),

      node("n1", "Server"),
      node("n2", "Server"),
      node("n3", "Server"),
      node("n4", "Server"),
      node("n5", "Server"),
      node("n6", "Server"),
      node("n7", "Server"),
      node("n8", "Server"),
      node("n9", "Server"),
      node("n10", "Server"),

      node("DSabath", "Person"),
      node("PSutton", "Person"),
      node("PKohn", "Person"),
      node("ECarlson", "Person"),

      node("d1", "Deployment", t0 = ts1, uname = "qa-dirsvc1"),
      node("d1", "Deployment", t0 = ts1, t1 = Some(ts2), uname = "qa-cass1"),
      node("d2", "Deployment", t0 = ts2, uname = "qa-cass2"),
      node("d1", "Deployment", t0 = ts1, t1 = Some(ts3), uname = "qa-asc1"),
      node("d2", "Deployment", t0 = ts3, uname = "qa-asc2"),
      node("d1", "Deployment", t0 = ts2, uname = "prod-cass1")

      //node("schema1", "Schema", uname = "schema1", other = JsonObject("schema"->Compact(jschema))),
      //node("dynamic1", "Dynamic", uname = "dynamic1", other = JsonObject("dynamic"->Compact(jdynamic)))
    )
    Future.sequence(cmds)
  }

  def relationships = {
    val cmds = Seq(
      relationship("asc", "Service", "Versions", "asc-v1", "Version"),
      relationship("asc", "Service", "Versions", "asc-v2", "Version"),
      relationship("asc", "Service", "Versions", "asc-v3", "Version"),
      relationship("cass", "Service", "Versions", "cass-v1", "Version"),
      relationship("cass", "Service", "Versions", "cass-v2", "Version"),
      relationship("dirsvc", "Service", "Versions", "dirsvc-v1", "Version"),

      relationship("asc-v2", "Version", "Uses", "dirsvc", "Service"),
      relationship("asc-v3", "Version", "Uses", "dirsvc", "Service"),
      relationship("asc-v3", "Version", "Uses", "cass", "Service"),

      relationship("qa", "Stack", "Clusters", "qa-asc", "Cluster"),
      relationship("qa", "Stack", "Clusters", "qa-cass", "Cluster"),
      relationship("qa", "Stack", "Clusters", "qa-dirsvc", "Cluster"),
      relationship("prod", "Stack", "Clusters", "prod-asc", "Cluster"),
      relationship("prod", "Stack", "Clusters", "prod-cass", "Cluster"),
      relationship("prod", "Stack", "Clusters", "prod-dirsvc", "Cluster"),

      relationship("asc", "Service", "ServiceClusters", "qa-asc", "Cluster"),
      relationship("asc", "Service", "ServiceClusters", "prod-asc", "Cluster"),
      relationship("cass", "Service", "ServiceClusters", "qa-cass", "Cluster"),
      relationship("cass", "Service", "ServiceClusters", "prod-cass", "Cluster"),
      relationship("dirsvc", "Service", "ServiceClusters", "qa-dirsvc", "Cluster"),
      relationship("dirsvc", "Service", "ServiceClusters", "prod-dirsvc", "Cluster"),

      // TODO add times to this relationship
      relationship("qa-asc", "Cluster", "Servers", "n1", "Server"),
      relationship("qa-asc", "Cluster", "Servers", "n2", "Server"),
      relationship("qa-asc", "Cluster", "Servers", "n3", "Server"),
      relationship("qa-cass", "Cluster", "Servers", "n4", "Server"),
      relationship("qa-cass", "Cluster", "Servers", "n5", "Server"),
      relationship("prod-cass", "Cluster", "Servers", "n6", "Server"),
      relationship("prod-cass", "Cluster", "Servers", "n7", "Server"),
      relationship("prod-cass", "Cluster", "Servers", "n8", "Server"),
      relationship("qa-dirsvc", "Cluster", "Servers", "n9", "Server"),
      relationship("qa-dirsvc", "Cluster", "Servers", "n10", "Server"),

      relationship("qa-dirsvc1", "Deployment", "DeployedFrom", "dirsvc-v1", "Version"),
      relationship("qa-cass1", "Deployment", "DeployedFrom", "cass-v1", "Version"),
      relationship("prod-cass1", "Deployment", "DeployedFrom", "cass-v1", "Version"),
      relationship("qa-cass2", "Deployment", "DeployedFrom", "cass-v2", "Version"),
      relationship("qa-asc1", "Deployment", "DeployedFrom", "asc-v1", "Version"),
      relationship("qa-asc2", "Deployment", "DeployedFrom", "asc-v2", "Version"),

      relationship("qa-dirsvc", "Cluster", "Deployments", "qa-dirsvc1", "Deployment"),
      relationship("qa-cass", "Cluster", "Deployments", "qa-cass1", "Deployment"),
      relationship("qa-cass", "Cluster", "Deployments", "qa-cass2", "Deployment"),
      relationship("qa-asc", "Cluster", "Deployments", "qa-asc1", "Deployment"),
      relationship("qa-asc", "Cluster", "Deployments", "qa-asc2", "Deployment"),
      relationship("prod-cass", "Cluster", "Deployments", "prod-cass1", "Deployment"),

      relationship("qa-dirsvc1", "Deployment", "DeployedBy", "PKohn", "Person"),
      relationship("qa-cass1", "Deployment", "DeployedBy", "DSabath", "Person"),
      relationship("qa-cass2", "Deployment", "DeployedBy", "DSabath", "Person"),
      relationship("qa-asc1", "Deployment", "DeployedBy", "PSutton", "Person"),
      relationship("qa-asc2", "Deployment", "DeployedBy", "ECarlson", "Person"),
      relationship("prod-cass1", "Deployment", "DeployedBy", "ECarlson", "Person"),

      relationship("qa-asc2", "Deployment", "DeploymentUses", "qa-dirsvc", "Cluster")

      //relationship("asc-v3", "Version", "HasSchema", "schema1", "Schema"),
      //relationship("asc-v3", "Version", "HasDynamic", "dynamic1", "Dynamic")

    )
    Future.sequence(cmds)
  }

}
