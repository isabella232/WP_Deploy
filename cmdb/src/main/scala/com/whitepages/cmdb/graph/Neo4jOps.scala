package com.whitepages.cmdb.graph

import akka.actor.{ActorRef, ActorRefFactory}
import com.persist.JsonOps._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import com.whitepages.framework.util.{ClassSupport, Util}
import scala.language.postfixOps

case class Neo4jOps(val util: Neo4jUtil) extends ClassSupport {

  import util._

  private[this] implicit val ec = system.dispatcher

  def services = {
    val f = cypherMatch(cypherNode("Service", "service"), Seq("service"))
    f map {
      case response =>
        val r = jgetArray(response, "data") map {
          case x => nameId(jgetObject(x, 0, "data"))
        }
        r
    }
  }

  def servicesSync = Await.result(services, 30 seconds)

  def stacks = {
    val f = cypherMatch(cypherNode("Stack", "stack"), Seq("stack"))
    f map {
      case response =>
        val r = jgetArray(response, "data") map {
          case x => nameId(jgetObject(x, 0, "data"))
        }
        r
    }
  }

  def stacksSync = Await.result(stacks, 30 seconds)

  def people = {
    val f = cypherMatch(cypherNode("Person", "person"), Seq("person"))
    //val f = cypher("match (n:Person) return n")
    f map {
      case response =>
        val r = jgetArray(response, "data") map {
          case x => nameId(jgetObject(x, 0, "data"))
        }
        r
    }
  }

  def peopleSync = Await.result(people, 30 seconds)

  def service(ref: String) = {
    val f = cypherMatch(cypherNode("Service", "service", props(ref)), Seq("service"))
    val f1 = cypherMatch(cypherNode("Service", props = props(ref)) + cypherRight("Versions") + cypherNode("Version", "version"), Seq("version"))
    val f2 = cypherMatch(cypherNode("Service", props = props(ref)) + cypherRight("ServiceClusters") + cypherNode("Cluster", "cluster"), Seq("cluster"))
    Future.sequence(Seq(f, f1, f2)) map {
      case Seq(response, response1, response2) =>
        val a = jgetArray(response, "data")
        val a1 = jgetArray(response1, "data")
        val a2 = jgetArray(response2, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val service = jget(a, 0, 0, "data")
          val versions = a1 map {
            case v =>
              jget(v, 0, "data")
          }
          val clusters = a2 map {
            case v =>
              jget(v, 0, "data")
          }
          JsonObject("service" -> service, "versions" -> versions, "clusters" -> clusters)
        }
    }
  }

  def serviceSync(ref: String) = Await.result(service(ref), 30 seconds)

  def serviceDeployments(ref: String) = {
    val f = cypherMatch(cypherNode("Service", props = props(ref)) + cypherRight("Versions") + cypherNode("Version") + cypherLeft("DeployedFrom") + cypherNode("Deployment", "deployment"), Seq("deployment"))
    f map {
      case response =>
        val a = jgetArray(response, "data")
        if (a.size == 0) {
          JsonArray()
        } else {
          for (d <- a) yield jgetObject(d, 0, "data")
        }
    }
  }

  def serviceDeploymentsSync(name: String) = Await.result(serviceDeployments(name), 30 seconds)

  def stack(ref: String) = {
    val f = cypherMatch(cypherNode("Stack", "stack", props(ref)), Seq("stack"))
    val f1 = cypherMatch(cypherNode("Stack", props = props(ref)) + cypherRight("Clusters") + cypherNode("Cluster", "cluster"), Seq("cluster"))
    Future.sequence(Seq(f, f1)) map {
      case Seq(response, response1) =>
        val a = jgetArray(response, "data")
        val a1 = jgetArray(response1, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val stack = jget(a, 0, 0, "data")
          val clusters = a1 map {
            case v =>
              jget(v, 0, "data")
          }
          JsonObject("stack" -> stack, "clusters" -> clusters)
        }
    }
  }

  def stackSync(ref: String) = Await.result(stack(ref), 30 seconds)

  def stackDeployments(ref: String) = {
    val f = cypherMatch(cypherNode("Stack", props = props(ref)) + cypherRight("Clusters") + cypherNode("Cluster") + cypherRight("Deployments") + cypherNode("Deployment", "deployment"), Seq("deployment"))
    f map {
      case response =>
        val a = jgetArray(response, "data")
        if (a.size == 0) {
          JsonArray()
        } else {
          for (d <- a) yield jgetObject(d, 0, "data")
        }
    }
  }

  def stackDeploymentsSync(name: String) = Await.result(stackDeployments(name), 30 seconds)

  def person(ref: String) = {
    val f = cypherMatch(cypherNode("Person", "person", props(ref)), Seq("person"))
    f map {
      case response =>
        val a = jgetArray(response, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val person = jget(a, 0, 0, "data")
          JsonObject("person" -> person)
        }
    }
  }

  def personSync(ref: String) = Await.result(person(ref), 30 seconds)

  def personDeployments(ref: String) = {
    val f = cypherMatch(cypherNode("Person", props = props(ref)) + cypherLeft("DeployedBy") + cypherNode("Deployment", "deployment"), Seq("deployment"))
    f map {
      case response =>
        val a = jgetArray(response, "data")
        if (a.size == 0) {
          JsonArray()
        } else {
          //for (d <- a) yield jgetString(d, 0, "data", "name")
          for (d <- a) yield jgetObject(d, 0, "data")
        }
    }
  }

  def personDeploymentsSync(name: String) = Await.result(personDeployments(name), 30 seconds)

  def cluster(ref: String) = {
    val compound = compoundRef(ref, Seq("Stack", "Cluster"), Seq("Clusters"), Seq("stack", "cluster"))
    val f = cypherMatch(compound, Seq("cluster", "stack"))
    val f1 = cypherMatch(compound + cypherRight("Servers") + cypherNode("Server", "server"), Seq("server"))
    val f2 = cypherMatch(compound + cypherRight("Deployments") + cypherNode("Deployment", "deployment"), Seq("deployment"))
    val f3 = cypherMatch(compound + cypherLeft("ServiceClusters") + cypherNode("Service", "service"), Seq("service"))
    Future.sequence(Seq(f, f1, f2, f3)) map {
      case Seq(response, response1, response2, response3) =>
        println(Pretty(response))
        val a = jgetArray(response, "data")
        val a1 = jgetArray(response1, "data")
        val a2 = jgetArray(response2, "data")
        val a3 = jgetArray(response3, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val cluster = jget(a, 0, 0, "data")
          val stack = jget(a, 0, 1, "data")
          val service = jget(a3, 0, 0, "data")
          val servers = a1 map {
            case v =>
              jget(v, 0, "data")
          }
          val deployments = a2 map {
            case v =>
              jget(v, 0, "data")
          }
          val uses = a2 map {
            case v =>
              jget(v, 0, "data")
          }
          JsonObject("cluster" -> cluster, "servers" -> servers, "stack" -> stack, "deployments" -> deployments, "service" -> service)
        }
    }

  }

  def clusterSync(name: String) = Await.result(cluster(name), 30 seconds)

  def version(ref: String) = {
    val compound = compoundRef(ref, Seq("Service", "Version"), Seq("Versions"), Seq("service", "version"))
    val f = cypherMatch(compound, Seq("version", "service"))
    val f1 = cypherMatch(compound + cypherRight("Uses") + cypherNode("Service", "service1"), Seq("service1"))
    val f2 = cypherMatch(compound + cypherLeft("DeployedFrom") + cypherNode("Deployment", "deployment"), Seq("deployment"))
    Future.sequence(Seq(f, f1, f2)) map {
      case Seq(response, response1, response2) =>
        val a = jgetArray(response, "data")
        val a1 = jgetArray(response1, "data")
        val a2 = jgetArray(response2, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val version = jgetObject(a, 0, 0, "data")
          val service = jget(a, 0, 1, "data")
          val deployments = a2 map {
            case v =>
              jget(v, 0, "data")
          }
          val uses = a1 map {
            case v =>
              jget(v, 0, "data")
          }
          val schema = jgetString(version, "schema")
          val dynamic = jgetString(version, "dynamic")
          val version1 = if (schema != "" && dynamic != "") {
            version ++ JsonObject("schema" -> Json(schema), "dynamic" -> Json(dynamic))
          } else {
            version
          }
          JsonObject("version" -> version1, "deployments" -> deployments, "service" -> service, "uses" -> uses)
        }
    }
  }

  def versionSync(name: String) = Await.result(version(name), 30 seconds)

  def server(ref: String) = {
    val f = cypherMatch(cypherNode("Stack", "stack") + cypherRight("Clusters") + cypherNode("Cluster", "cluster") +
      cypherRight("Servers") + cypherNode("Server", "server", props(ref)), Seq("server", "cluster", "stack"))
    f map {
      case response =>
        val a = jgetArray(response, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val server = jget(a, 0, 0, "data")
          val cluster = jget(a, 0, 1, "data")
          val stack = jget(a, 0, 2, "data")
          JsonObject("server" -> server, "cluster" -> cluster, "stack" -> stack)
        }
    }
  }

  def serverSync(ref: String) = Await.result(server(ref), 30 seconds)

  def deployment(ref: String) = {
    val compound = compoundRef(ref, Seq("Stack", "Cluster", "Deployment"), Seq("Clusters", "Deployments"), Seq("stack", "cluster", "deployment"))
    val f = cypherMatch(compound + cypherRight("DeployedFrom") + cypherNode("Version", "version") + cypherLeft("Versions") + cypherNode("Service", "service"),
      Seq("deployment", "version", "cluster", "service", "stack"))
    val f1 = cypherMatch(compound + cypherRight("DeployedBy") + cypherNode("Person", "person"), Seq("person"))
    val f2 = cypherMatch(compound + cypherRight("DeploymentUses") + cypherNode("Cluster", "cluster1"), Seq("cluster1"))
    Future.sequence(Seq(f, f1, f2)) map {
      case Seq(response, response1, response2) =>
        val a = jgetArray(response, "data")
        val a1 = jgetArray(response1, "data")
        val a2 = jgetArray(response2, "data")
        if (a.size == 0) {
          JsonObject()
        } else {
          val deployment = jget(a, 0, 0, "data")
          val version = jget(a, 0, 1, "data")
          val cluster = jget(a, 0, 2, "data")
          val service = jget(a, 0, 3, "data")
          val stack = jget(a, 0, 4, "data")
          val person = jget(a1, 0, 0, "data")
          val uses = a2 map {
            case v =>
              jget(v, 0, "data")
          }
          JsonObject("deployment" -> deployment, "version" -> version, "cluster" -> cluster,
            "service" -> service, "stack" -> stack, "person" -> person, "uses" -> uses)
        }
    }
  }

  def deploymentSync(ref: String) = Await.result(deployment(ref), 30 seconds)

  def undeploy(id: String, t1: String) = {
    // TODO make transactional
    cypherMatch(cypherNode("Deployment", "deployment", props(id)) + s" set deployment.t1='$t1'", Seq("deployment"))
  }

  def undeploySync(id: String, t1: String) = Await.result(undeploy(id, t1), 30 seconds)

  def deploy(name: String, t0: String, clusterId: String, versionId: String, personId: String, uses: JsonArray) = {
    // TODO make transactional
    val f = createNode(name, t0, "Deployment")
    val f1 = f flatMap {
      case d =>
        println(Pretty(d))
        val id = jgetString(d, "data", 0, 0, "data", "id")
        println("cid=" + id)
        val f1 = createRelationship(clusterId, "Cluster", "Deployments", id, "Deployment")
        val f2 = createRelationship(id, "Deployment", "DeployedFrom", versionId, "Version")
        val f3 = createRelationship(id, "Deployment", "DeployedBy", personId, "Person")
        val f4s = for (use <- uses) yield {
          createRelationship(id, "Deployment", "DeploymentUses", jgetString(use), "Cluster")
        }
        val f4 = Future.sequence(f4s)
        Future.sequence(Seq(f1, f2, f3, f4))
    }
    f1
  }

  def deploySync(name: String, t0: String, clusterId: String, versionId: String, personId: String, uses: JsonArray) = {
    Await.result(deploy(name, t0, clusterId, versionId, personId, uses), 30 seconds)
  }

  def stop() {
    util.stop
  }

}
